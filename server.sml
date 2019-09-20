structure NetServer :
sig

type sock = (INetSock.inet, Socket.active Socket.stream) Socket.sock

datatype ('c, 'd) settings = Settings of {
  handler      : ('c option * 'd option) -> sock -> unit,
  port         : int,
  host         : string,
  acceptQueue  : int,
  workers      : int,
  maxRequests  : int,
  reuseport    : bool,
  workerHook   : ((unit -> 'c) * ('c -> unit)) option,
  connectHook  : ((unit -> 'd) * ('d -> unit)) option,
  logger       : string -> unit
}

val run: ('c, 'd) settings -> unit

val needStop: unit -> bool

val read  : sock * int    * Time.time option -> string
val write : sock * string * Time.time option -> bool

end
=
struct

type sock = (INetSock.inet, Socket.active Socket.stream) Socket.sock

open NetServer

datatype ('c, 'd) settings = Settings of {
  handler      : ('c option * 'd option) -> sock -> unit,
  port         : int,
  host         : string,
  acceptQueue  : int,
  workers      : int,
  maxRequests  : int,
  reuseport    : bool,
  workerHook   : ((unit -> 'c) * ('c -> unit)) option,
  connectHook  : ((unit -> 'd) * ('d -> unit)) option,
  logger       : string -> unit
}

type listenSocket = (INetSock.inet, Socket.passive Socket.stream) Socket.sock
datatype ListenSocket = ListenSocket of listenSocket | GetListenSocket of unit -> listenSocket



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
        Socket.listen (sock, (#acceptQueue settings));
        sock
      end

    val maybeListenSock = if reuseport then GetListenSocket listen else ListenSocket (listen ())

    fun doListen maybeListenSock =
      let
        val listenSock = case maybeListenSock of ListenSocket sock => sock | GetListenSocket f => f ()
        val handler = #handler settings

        val workerHook = #workerHook settings
        val workerHookData = case workerHook of NONE => NONE | SOME (init, cleanup) => SOME (init ())

        val connectHook = #connectHook settings

        val maxRequests = #maxRequests settings

        fun doAccept request_cnt = case accept listenSock of
            NONE => if needStop () then () else doAccept request_cnt
          | SOME (sock, _) =>
          let
            val connectHookData = case connectHook of NONE => NONE | SOME (init, cleanup) => SOME (init ())
          in
            Socket.Ctl.setKEEPALIVE (sock, true);
            handler (workerHookData, connectHookData) sock handle exc => logger ("function handler raised an exception: " ^ exnMessage exc);
            case connectHook of NONE => () | SOME (init, cleanup) => cleanup (valOf connectHookData);
            Socket.close sock;
            if needStop () then () else
            if maxRequests = 0 then doAccept request_cnt else
            if request_cnt < maxRequests then doAccept (request_cnt + 1) else logger ("Max request count achieved.")
          end

      in
        doAccept 1;
        case workerHook of NONE => () | SOME (init, cleanup) => cleanup (valOf workerHookData)
      end
  in
    runWithN logger (#workers settings) doListen maybeListenSock;
    case maybeListenSock of ListenSocket sock => Socket.close sock | _ => ()
  end


fun run (Settings settings) = run' run'' settings handle exc => (#logger settings) ("function run raised an exception: " ^ exnMessage exc)

end
