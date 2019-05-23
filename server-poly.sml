structure NetServer =
struct

val stop = ref false
val quit = ref false

fun needStop () = !stop orelse !quit

fun run' f x = (
  Signal.signal (Posix.Signal.pipe, Signal.SIG_IGN);
  Signal.signal (Posix.Signal.term, Signal.SIG_HANDLE (fn _ => (stop := true; Thread.Thread.broadcastInterrupt ())));
  Signal.signal (Posix.Signal.quit, Signal.SIG_HANDLE (fn _ => (quit := true; Thread.Thread.broadcastInterrupt ())));
  f x
)


fun accept socket =
  let
    val sd = Socket.sockDesc socket
    fun doit socket = case Socket.select { rds = [sd], wrs = [], exs = [], timeout = NONE } of
        { rds = [sd], wrs = [], exs = [] } =>
          (case Socket.acceptNB socket of NONE (* Other worker was first *) => doit socket | r => r)
      | _ => if needStop () then NONE else doit socket
  in
    doit socket handle Thread.Thread.Interrupt => if needStop () then NONE else doit socket | exc => raise exc
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
      | exc as OS.SysErr (s, SOME e) => if OS.errorName e = "ECONNRESET" then "" else raise exc
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
      | exc as OS.SysErr (s, SOME e) => if OS.errorName e = "EPIPE" then false else raise exc
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

  val m = mutex ()
  val c = conditionVar ()
  val cnt = ref 0

  fun doFork n f x =
    let
      fun doit 0 = ()
        | doit n = (
            cnt := !cnt + 1;
            fork (fn () => (f x; lock m; signal c; cnt := !cnt - 1; unlock m), [EnableBroadcastInterrupt true]);
            doit (n - 1)
          )
    in
      lock m;
      doit n;
      unlock m
    end

  fun doWait f x  =
    while !cnt > 0 do (
      wait(c, m);
      if needStop () then () else (unlock m; doFork 1 f x)
    ) handle Interrupt => doWait f x | exc => raise exc

in
  fun runWithN logger n f x =
    if n > 0
    then (doFork n f x; doWait f x)
    else f x
end

end
