(* echo ping | nc localhost 5000 *)

fun logger msg = print ((Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeUniv(Time.now()))) ^ "\t" ^ msg ^ "\n")

fun main () =
  let

    val timeout = Time.fromSeconds 10

    fun read socket = NetServer.read (socket, 1024, (SOME timeout))
    fun write (socket, text) = NetServer.write (socket, text, (SOME timeout))

    fun handler (workerHookData, connectHookData) socket = (
      logger "Hello, socket.";
      case connectHookData of NONE => () | SOME data => print data;
      let
        val r = read socket
      in
        if r = ""
        then (if NetServer.needStop () then false else write (socket, "timeout\n"))
        else (print r; write (socket, "pong\n"))
      end;
      logger "BY, socket."
    )

    val settings = NetServer.Settings {
      handler        = handler,
      port           = 5000,
      host           = "*",
      acceptQueue    = 128,
      workers        = 3,
      maxRequests    = 1000, (* ToDo *)
      reuseport      = false,
      workerHook     = SOME ( (fn () => logger "Worker init hook."),  (fn _  => logger "Worker cleanup hook.") ),
      connectHook    = SOME ( (fn () => (logger "Connect init hook."; "It's connect hook data.\n")), (fn _  => logger "Connect cleanup hook.") ),
      logger         = logger
    }

  in
    logger "Start.";
    NetServer.run settings
  end
