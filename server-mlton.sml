structure NetServer =
struct

val stop = ref false

fun needStop () = !stop

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
          if e = Posix.Error.intr then (if !stop then NONE else doit socket) else
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

  fun setHandlerForTermSignal false = (
      MLton.Signal.setHandler(Posix.Signal.term, MLton.Signal.Handler.simple (fn () => stop := true))
    )
    | setHandlerForTermSignal true = ( (* send signal to group *)
      MLton.Signal.setHandler(Posix.Signal.term, MLton.Signal.Handler.simple (fn () => (
        stop := true;
        (* print ("Got TERM signal for " ^ (pidToString (Posix.ProcEnv.getpid ())) ^ ", main pid is " ^ (pidToString main_pid) ^ ".\n"); *)
        if main_pid = Posix.ProcEnv.getpid ()
        then List.app (fn pid => Posix.Process.kill (Posix.Process.K_PROC pid, Posix.Signal.term)) (!child_pids)
        else ()
      )))
    )

  fun doFork 0 f x = ()
    | doFork n f x =
        case Posix.Process.fork () of
             NONE => (
                child_pids := [];
                print ("I am child, my PID is " ^ ( myPidAsString () ) ^ ".\n");
                f x;
                Posix.Process.exit 0w0
                )
           | SOME pid => (
               child_pids := pid::(!child_pids);
               doFork (n - 1) f x
             )

  fun wait f x =
    let
      val (pid, _) = Posix.Process.wait ()
    in
      (* print ("Stoped " ^ pidToString pid ^ "\n"); *)
      child_pids := List.filter (fn p => p <> pid) (!child_pids);
      if !stop then () else doFork 1 f x;
      if null (!child_pids) then () else wait f x
    end

in
  fun runWithN n f x =
    if n > 0
    then (
      setHandlerForTermSignal true;
      print ("My PID is " ^ ( myPidAsString () ) ^ ".\n");
      doFork n f x;
      wait f x
    )
    else (
      setHandlerForTermSignal false;
      f x
    )
end

end
