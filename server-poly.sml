structure NetServer =
struct

val stop = ref false

fun needStop () = !stop

fun run' f x = (
  Signal.signal (Posix.Signal.pipe, Signal.SIG_IGN);
  Signal.signal (Posix.Signal.term, Signal.SIG_HANDLE (fn _ => (stop := true; Thread.Thread.broadcastInterrupt ())));
  f x
)


fun accept socket =
  let
    val sd = Socket.sockDesc socket
    fun doit socket = case Socket.select { rds = [sd], wrs = [], exs = [], timeout = NONE } of
        { rds = [sd], wrs = [], exs = [] } =>
          (case Socket.acceptNB socket of NONE (* Other worker was first *) => doit socket | r => r)
      | _ => if !stop then NONE else doit socket
  in
    doit socket handle Thread.Thread.Interrupt => if !stop then NONE else doit socket | exc => raise exc
  end


(* Exceptions do not used for read and write, since functions can be called from callback of C function *)

(* return "" if timeout or stop *)
fun read (socket, chunksize, (timeout:Time.time option)) =
  let
    val sd = Socket.sockDesc socket
    fun doit timeout = case Socket.select { rds = [sd], wrs = [], exs = [], timeout = timeout } of
        { rds = [sd], wrs = [], exs = [] } => Byte.bytesToString (Socket.recvVec (socket, chunksize))
      | _ => ""
  in
    doit timeout handle
        Thread.Thread.Interrupt => if !stop then "" else doit timeout
      | OS.SysErr (_, SOME ECONNRESET) => ""
      | exc => raise exc
  end


(* return false if timeout or stop *)
fun write (socket, text, (timeout:Time.time option)) =
  let
    val sd = Socket.sockDesc socket
    val data = Word8VectorSlice.full (Byte.stringToBytes text)

    fun doit (data, timeout) = case Socket.select { rds = [], wrs = [sd], exs = [], timeout = timeout } of
        { rds = [], wrs = [sd], exs = [] } =>
          let
            val n = Socket.sendVec (socket, data)
          in
            if n = Word8VectorSlice.length data then true else
            doit ((Word8VectorSlice.subslice (data, n, NONE)), timeout)
          end
      | _ => false
  in
    doit (data, timeout) handle
        Thread.Thread.Interrupt => if !stop then false else doit (data, timeout)
      | OS.SysErr (_, SOME EPIPE) => false
      | exc => raise exc
  end


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
        val _ = fork(fn() => (f x; lock m; signal c; unlock m), [EnableBroadcastInterrupt true])
      in
        doFork (n-1) f x ((m, c)::tm)
      end

  fun doWait [] = ()
    | doWait (all as ((m, c)::xs)) = (wait(c, m); doWait xs) handle Interrupt => doWait all | exc => raise exc
in
  fun runWithN n f x =
    if n > 1
    then doWait (doFork n f x [])
    else f x
end

end
