structure NetServer :
sig
  datatype ('a, 'b) settings = Settings of {
    handler      : ('a, 'b) Socket.sock -> unit,
    port         : int,
    host         : string,
    accept_queue : int,
    workers      : int,
    max_requests : int,
    reuseport    : bool,
    logger       : string -> unit
  }
  val run: (INetSock.inet, Socket.active Socket.stream) settings -> unit
end
=
struct

open NetServer

datatype ('a, 'b) settings = Settings of {
  handler      : ('a, 'b) Socket.sock -> unit,
  port         : int,
  host         : string,
  accept_queue : int,
  workers      : int,
  max_requests : int,
  reuseport    : bool,
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

        fun do_accept () =
          let
            val (sock, _) = Socket.accept (listen_sock)
          in
            Socket.Ctl.setKEEPALIVE (sock, true);
            (handler sock) handle exc => logger ("function handler raised an exception: " ^ exnMessage exc);
            do_accept ()
          end
      in
        do_accept ()
      end
  in
    runWithN (#workers settings) do_listen maybe_listen_sock;
    case maybe_listen_sock of ListenSocket sock => Socket.close sock | _ => ();
    logger "The End."
  end


fun run (Settings settings) = run' run'' settings handle exc => (#logger settings) ("function run raised an exception: " ^ exnMessage exc)

end
