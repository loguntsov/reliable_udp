-module(rudp_listener_worker).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-include("packet_type.hrl").
-include("listener.hrl").
-include("logger.hrl").

-behaviour(gen_server).

%% API
-export([
  start_link/1,
  process/3
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  listener :: rudp_listener:listener()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Listener) ->
  gen_server:start_link(?MODULE, [ Listener ], []).

process(Pid, Ip, Packet) ->
  rudp_overload:protect(Pid, 1000, fun() ->
    gen_server:cast(Pid, { process, Ip, Packet })
  end).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ Listener ]) ->
  {ok, #state{
    listener = Listener
  }}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({ process, IP, Packet}, State) ->
  %% ?INFO("Received packet ~p ~p", [  IP, Packet ]),
  case rudp_packet:connection_id(Packet) of
    { ok, _Type, ConnId } ->
      %% ?INFO("Recognized packet ~p ~p", [ _Type, ConnId ]),
      case rudp_connection:get_pid(IP, ConnId) of
        { ok, Pid } ->
          rudp_receiver:process_packet(Pid, Packet);
        undefined ->
          case rudp_connection:get_pid(undefined, ConnId) of
            { ok, Pid } ->
              rudp_receiver:set_ip(Pid, IP),
              rudp_receiver:process_packet(Pid, Packet);
            undefined ->
              ?INFO("Packet skiped ~p ~p", [ IP, size(Packet) ])
          end
      end;
    { undefined, ?CONNECT } ->
      case rudp_packet:parse(Packet) of
        #connect_packet{} = ParsedPacket ->
          %% ?INFO("Connection packet ~p ", [ ParsedPacket ]),
          AcceptorPid = (State#state.listener)#listener.acceptor,
          rudp_listener_acceptor:fire(AcceptorPid, IP, ParsedPacket),
          ok;
        false ->
          ok
      end
  end,
  { noreply, State };
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
