fun logger msg = print ((Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeUniv(Time.now()))) ^ "\t" ^ msg ^ "\n")

fun main () =
  let

    fun handler socket = (
      logger "Hello, socket.";
      print (Byte.bytesToString(Socket.recvVec (socket, 1024)));
      Socket.sendVec (socket, Word8VectorSlice.full (Byte.stringToBytes "pong\n"));
      Socket.close socket
    )

    val settings = NetServer.Settings {
      handler      = handler,
      port         = 8080,
      host         = "*",
      accept_queue = 10,
      workers      = 0,
      max_requests = 1000, (* ToDo *)
      reuseport    = false,
      logger       = logger
    }

  in
    logger "Start.";
    NetServer.run settings
  end
