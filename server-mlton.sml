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
    fun doit socket = case Socket.select { rds = [(Socket.sockDesc socket)], wrs = [], exs = [], timeout = NONE } of
        { rds = [sd], wrs = [], exs = [] } =>
        (case Socket.acceptNB socket of NONE (* Other worker was first *) => doit socket | r => r)
      | _ => NONE
  in
    doit socket handle OS.SysErr ("Interrupted system call", _) => if !stop then NONE else doit socket | exc => raise exc
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
    if n > 1
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
