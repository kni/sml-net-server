structure NetServer :
sig

datatype ('a, 'b, 'c, 'd) settings = Settings of {
  handler      : ('c option * 'd option) -> ('a, 'b) Socket.sock -> unit,
  port         : int,
  host         : string,
  accept_queue : int,
  workers      : int,
  max_requests : int,
  reuseport    : bool,
  worker_hook  : ((unit -> 'c) * ('c -> unit)) option,
  connect_hook : ((unit -> 'd) * ('d -> unit)) option,
  logger       : string -> unit
}

val run: (INetSock.inet, Socket.active Socket.stream, 'c, 'd) settings -> unit

end
=
struct

open NetServer

datatype ('a, 'b, 'c, 'd) settings = Settings of {
  handler      : ('c option * 'd option) -> ('a, 'b) Socket.sock -> unit,
  port         : int,
  host         : string,
  accept_queue : int,
  workers      : int,
  max_requests : int,
  reuseport    : bool,
  worker_hook  : ((unit -> 'c) * ('c -> unit)) option,
  connect_hook : ((unit -> 'd) * ('d -> unit)) option,
  logger       : string -> unit
}


datatype ('a, 'b) ListenSocket = ListenSocket of ('a, 'b) Socket.sock | GetListenSocket of unit -> ('a, 'b) Socket.sock



val sockToEvFD : ('a, 'b) Socket.sock -> int = fn sock => (SysWord.toInt o Posix.FileSys.fdToWord o Option.valOf o Posix.FileSys.iodToFD o Socket.ioDesc) sock


fun run'' (settings as {host = host, port = port, reuseport = reuseport, logger = logger, ...}) =
  let

    val addr = if host = "*" then INetSock.any port else
      case NetHostDB.fromString host of NONE => INetSock.any port | SOME h => INetSock.toAddr(h, port)

    fun listen () =
      let
        val sock = INetSock.TCP.socket ()
        val fd = sockToEvFD sock
      in
        logger ("Listening on " ^ host ^ ":" ^ (Int.toString port) ^ ".");
        Socket.Ctl.setREUSEADDR (sock, true);
        if reuseport then setsockopt_REUSEPORT fd else ();
        Socket.bind (sock, addr);
        Socket.listen (sock, (#accept_queue settings));
        sock
      end

    val maybe_listen_sock = if reuseport then GetListenSocket listen else ListenSocket (listen ())

    fun do_listen maybe_listen_sock =
      let
        val listen_sock = case maybe_listen_sock of ListenSocket sock => sock | GetListenSocket f => f ()
        val handler = #handler settings

        val worker_hook = #worker_hook settings
        val worker_hook_data = case worker_hook of NONE => NONE | SOME (init, cleanup) => SOME (init ())

        val connect_hook = #connect_hook settings

        fun do_accept () =
          let
            val (sock, _) = Socket.accept (listen_sock)
            val connect_hook_data = case connect_hook of NONE => NONE | SOME (init, cleanup) => SOME (init ())
          in
            Socket.Ctl.setKEEPALIVE (sock, true);
            handler (worker_hook_data, connect_hook_data) sock handle exc => logger ("function handler raised an exception: " ^ exnMessage exc);
            case connect_hook of NONE => () | SOME (init, cleanup) => cleanup (valOf connect_hook_data);
            Socket.close sock;
            do_accept ()
          end

      in
        do_accept ();
        case worker_hook of NONE => () | SOME (init, cleanup) => cleanup (valOf worker_hook_data)
      end
  in
    runWithN (#workers settings) do_listen maybe_listen_sock;
    case maybe_listen_sock of ListenSocket sock => Socket.close sock | _ => ();
    logger "The End."
  end


fun run (Settings settings) = run' run'' settings handle exc => (#logger settings) ("function run raised an exception: " ^ exnMessage exc)

end
