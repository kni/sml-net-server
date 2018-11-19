structure NetServer =
struct

fun run' f x = (
  Signal.signal(Posix.Signal.pipe, Signal.SIG_IGN);
  f x
)



exception Socket of string

local
  open Foreign
  val libc = loadExecutable ()
  val setsockopt_ffi = buildCall5 ((getSymbol libc "setsockopt"), (cInt, cInt, cInt, cConstStar cInt, cInt), cInt) 
in
  fun setsockopt_REUSEPORT fd =
    if setsockopt_ffi (fd, OS_Constants.SOL_SOCKET, OS_Constants.SO_REUSEPORT, 1, 4) = ~1
    then raise Socket "Cannot set SO_REUSEPORT option on socket"
    else ()
end



local
  open Thread 
  open Thread Mutex ConditionVar

  fun doFork 0 f x tm = tm
    | doFork n f x tm =
      let
        val m = mutex ()
        val c = conditionVar ()
        val _ = lock m
        val _ = fork(fn() => (f x; lock m; signal c; unlock m), [])
      in
        doFork (n-1) f x ((m, c)::tm)
      end

  fun doWait []              = ()
    | doWait ((m, c)::xs) = wait(c, m)
in
  fun runWithN n f x =
    if n > 1
    then doWait (doFork n f x [])
    else f x
end

end
