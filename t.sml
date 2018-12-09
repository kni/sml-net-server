(* echo ping | nc localhost 5000 *)

fun logger msg = print ((Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeUniv(Time.now()))) ^ "\t" ^ msg ^ "\n")

fun main () =
  let

    fun handler (worker_hook_data, connect_hook_data) socket = (
      logger "Hello, socket.";
      case connect_hook_data of NONE => () | SOME data => print data;
      print (Byte.bytesToString(Socket.recvVec (socket, 1024)));
      Socket.sendVec (socket, Word8VectorSlice.full (Byte.stringToBytes "pong\n"));
      logger "BY, socket."
    )

    val settings = NetServer.Settings {
      handler        = handler,
      port           = 5000,
      host           = "*",
      accept_queue   = 10,
      workers        = 0,
      max_requests   = 1000, (* ToDo *)
      reuseport      = false,
      worker_hook    = SOME ( (fn () => logger "Worker init hook."),  (fn _  => logger "Worker cleanup hook.") ),
      connect_hook   = SOME ( (fn () => (logger "Connect init hook."; "It's connect hook data.\n")), (fn _  => logger "Connect cleanup hook.") ),
      logger         = logger
    }

  in
    logger "Start.";
    NetServer.run settings
  end
