-module(gen_rudp).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  start_listener/2, start_listener/1,
  stop_listener/1,
  connect/4, connect/5,
  close/1,
  accept/2, accept/1,
  is_alive/1, is_connected/1,
  controlling_process/2,
  async_send_binary/2, sync_send_binary/3,
  send_udp/2
]).

-include("listener.hrl").
-include("socket.hrl").
-include("packet_type.hrl").
-include("priorities.hrl").

% -define(WORKERS, rudp_app:env(workers_for_each_port)).
-define(WORKERS, 1).
-define(TIMEOUT, 5000).

-spec start_listener( Port :: pos_integer(), Opts :: proplists:proplist() ) -> { ok, Lestener :: rudp_listener:listener() }.
start_listener(Port, Opts) ->
  { ok, Pid } = rudp_listener:start_link(?WORKERS, Port, Opts),
  { ok, Listener } = rudp_listener_registrar:lookup(Pid),
  { ok, Listener }.

start_listener(Port) ->
  start_listener(Port, []).

-spec stop_listener( pid() | rudp_listener:listener() | pos_integer()) -> ok | undefined.
stop_listener(Pid) when is_pid(Pid) ->
  rudp_listener:stop(Pid);
stop_listener(#listener{ pid = Pid }) ->
  stop_listener(Pid);
stop_listener(Port) when is_integer(Port) ->
  case rudp_listener_registrar:lookup(Port) of
    { ok, Listener } ->
      stop_listener(Listener);
    undefined -> undefined
  end.

-spec connect( pid(), inet:ip_address(), pos_integer(), [], pos_integer() ) -> { ok, rudp_socket:socket() } |  { error, Reason :: term() }.
connect(Listener, Address, Port, Options, Timeout) ->
  { ok, Protocol } = rudp_protocol:start_link(Listener),
  case rudp_protocol:connect(Protocol, Address, Port, Options, Timeout) of
    ok ->
      { ok, Socket } = rudp_protocol:get_socket(Protocol),
      { ok, Socket };
    Any ->
      rudp_protocol:destroy(Protocol),
      Any
  end.

connect(Listener, Address, Port, Options) ->
  connect(Listener, Address, Port, Options, rudp_app:env(connection_timeout)).

-spec close(rudp_socket:socket()) -> ok.
close(Socket) ->
  rudp_protocol:destroy(Socket#rudp_socket.protocol).

-spec accept(Listener :: rudp_listener:listener(), Timeout :: pos_integer() | infinity) -> { ok, rudp_socket:socket() } | { error, Reason :: atom() }.
accept(Listener, Timeout) ->
  AcceptorPid = Listener#listener.acceptor,
  case rudp_listener_acceptor:accept(AcceptorPid, Timeout) of
    { error, _ } = Error -> Error;
    { ok, IP, ParsedPacket = #connect_packet{} } ->
      { ok, ProtocolPid } = rudp_protocol:start_link(Listener),
      case rudp_protocol:accept(ProtocolPid, IP, ParsedPacket, rudp_app:env(connection_timeout)) of
        ok ->
          { ok, Socket } = rudp_protocol:get_socket(ProtocolPid),
          { ok, Socket };
        Any ->
          rudp_protocol:destroy(ProtocolPid),
          Any
      end
  end.

accept(Listener) ->
  accept(Listener, infinity).

-spec is_alive( rudp_socket:socket() ) -> boolean().
is_alive(Socket) ->
  ProtocolPid = Socket#rudp_socket.protocol,
  erlang:is_process_alive(ProtocolPid).

-spec is_connected( rudp_socket:socket() ) -> boolean().
is_connected(Socket) ->
  ProtocolPid = Socket#rudp_socket.protocol,
  case erlang:is_process_alive(ProtocolPid) of
    false -> false;
    true ->
      state_connected =:= rudp_protocol:get_state_atom(ProtocolPid)
  end.

-spec controlling_process(Socket :: rudp_socket:socket(), Pid :: pid) -> ok | { error, Reason :: atom() }.
controlling_process(Socket, Pid) ->
  rudp_protocol:controlling_process(Socket#rudp_socket.protocol, Pid).

async_send_binary(Socket, Binary) ->
  rudp_socket:sender(Socket, fun(Pid) ->
    rudp_sender:async_send_binary(Pid, ?PRIORITY_LOW, binary, Binary)
  end).

sync_send_binary(Socket, Binary, Timeout) ->
  rudp_socket:sender(Socket, fun(Pid) ->
    rudp_sender:sync_send_binary(Pid, ?PRIORITY_LOW, binary, Binary, Timeout)
  end).

send_udp(_Socket, Binary) when size(Binary) > 1024 -> { error, big_packet };
send_udp(Socket, Binary) ->
  rudp_socket:sender(Socket, fun(Pid) ->
    rudp_sender:async_send_binary(Pid, ?PRIORITY_MIDDLE, upd, Binary)
  end).


