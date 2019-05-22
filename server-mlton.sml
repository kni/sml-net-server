structure NetServer =
struct

val stop = ref false
val quit = ref false

fun needStop () = !stop orelse !quit

fun run' f x = (
  MLton.Signal.setHandler(Posix.Signal.pipe, MLton.Signal.Handler.ignore);
  f x
)


fun accept socket =
  let
    val sd = Socket.sockDesc socket
    fun doit socket = case Socket.select { rds = [sd], wrs = [], exs = [], timeout = NONE } of
        { rds = [sd], wrs = [], exs = [] } =>
        (case Socket.acceptNB socket of NONE (* Other worker was first *) => doit socket | r => r)
      | _ => NONE
  in
    doit socket handle
        OS.SysErr (s, SOME e) =>
          if e = Posix.Error.intr then (if needStop () then NONE else doit socket) else
          raise OS.SysErr (s, SOME e)
      | exc => raise exc
  end


(* Exceptions do not used for read and write, since functions can be called from callback of C function *)

(* return "" if timeout or stop *)
fun read (socket, chunksize, (timeout:Time.time option)) =
  let
    val sd = Socket.sockDesc socket
    fun doit () = case Socket.select { rds = [sd], wrs = [], exs = [], timeout = timeout } of
        { rds = [sd], wrs = [], exs = [] } => Byte.bytesToString (Socket.recvVec (socket, chunksize))
      | _ => ""
  in
    doit () handle
        exc as OS.SysErr (s, SOME e) =>
          if e = Posix.Error.intr then (if !stop then "" else doit ()) else
          if Posix.Error.errorName e = "connreset" then "" else
          raise exc
      | exc => raise exc
  end


(* return false if timeout or stop *)
fun write (socket, text, (timeout:Time.time option)) =
  let
    val sd = Socket.sockDesc socket
    val data = Word8VectorSlice.full (Byte.stringToBytes text)

    fun doit data = case Socket.select { rds = [], wrs = [sd], exs = [], timeout = timeout } of
        { rds = [], wrs = [sd], exs = [] } =>
          let
            val n = Socket.sendVec (socket, data)
          in
            if n = Word8VectorSlice.length data then true else
            doit (Word8VectorSlice.subslice (data, n, NONE))
          end
      | _ => false
  in
    doit data handle
        exc as OS.SysErr (s, SOME e) =>
          if e = Posix.Error.intr then (if !stop then false else doit data) else
          if e = Posix.Error.pipe then false else
          raise exc
      | exc => raise exc
  end


exception Socket of string

local
  val setsockopt_ffi = _import "setsockopt": int * int * int * int ref * int-> int;
in
  fun setsockopt_REUSEPORT fd =
    if setsockopt_ffi (fd, OS_Constants.SOL_SOCKET, OS_Constants.SO_REUSEPORT, (ref 1), 4) = ~1
    then raise Socket "Cannot set SO_REUSEPORT option on socket"
    else ()
end



local
  val pidToString = LargeInt.toString o SysWord.toLargeInt o Posix.Process.pidToWord
  fun myPidAsString () = pidToString (Posix.ProcEnv.getpid ())

  val main_pid = Posix.ProcEnv.getpid ()

  val child_pids = ref []


  fun sendSignalToChild signal =
    if main_pid = Posix.ProcEnv.getpid ()
    then List.app (fn pid => Posix.Process.kill (Posix.Process.K_PROC pid, signal)) (!child_pids)
    else ()


  fun setHandlersForSignals false = (
      MLton.Signal.setHandler (Posix.Signal.term, MLton.Signal.Handler.simple (fn () => stop := true));
      MLton.Signal.setHandler (Posix.Signal.quit, MLton.Signal.Handler.simple (fn () => quit := true))
    )
    | setHandlersForSignals true = ( (* send signal to group *)
      MLton.Signal.setHandler (Posix.Signal.term, MLton.Signal.Handler.simple (fn () => (
        stop := true;
        (* print ("Got TERM signal for " ^ (pidToString (Posix.ProcEnv.getpid ())) ^ ", main pid is " ^ (pidToString main_pid) ^ ".\n"); *)
        sendSignalToChild Posix.Signal.term
      )));
      MLton.Signal.setHandler (Posix.Signal.quit, MLton.Signal.Handler.simple (fn () => (
        quit := true;
        sendSignalToChild Posix.Signal.quit
      )))
    )

  fun doFork logger 0 f x = ()
    | doFork logger n f x =
        case Posix.Process.fork () of
             NONE => (
                child_pids := [];
                logger ("I am child, my PID is " ^ ( myPidAsString () ) ^ ".");
                f x;
                Posix.Process.exit 0w0
                )
           | SOME pid => (
               child_pids := pid::(!child_pids);
               doFork logger (n - 1) f x
             )


  fun wait logger f x =
    let
      val (pid, _) = Posix.Process.wait ()
    in
      (* logger ("Stoped " ^ pidToString pid); *)
      child_pids := List.filter (fn p => p <> pid) (!child_pids);
      if needStop () then () else doFork logger 1 f x;
      if null (!child_pids) then () else wait logger f x
    end

in
  fun runWithN logger n f x =
    if n > 0
    then (
      setHandlersForSignals true;
      logger ("My PID is " ^ ( myPidAsString () ) ^ ".");
      doFork logger n f x;
      wait logger f x
    )
    else (
      setHandlersForSignals false;
      f x
    )
end

end
