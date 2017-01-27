-module(rudp_protocol).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behaviour(gen_server).

%% API
-export([
  start_link/1, close/2, destroy/1,
  connect/4, connect/5,
  controlling_process/2,
  accept/4,
  incomming_packet/2,
  get_socket/1,
  get_state_atom/1
]).

-include("listener.hrl").
-include("logger.hrl").
-include("packet_type.hrl").
-include("socket.hrl").
-include("priorities.hrl").

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(PACKETS_COUNT, 65535).
-define(PROTOCOL_VERSION, 1).
-define(TIMEOUT_DECREASE_STEP, 10).

-record(state_closed, {}).
-record(state_waiting_confirmation_connection_sent, { from, address, port }).
-record(state_establish_connection, {}).
-record(state_connected, {
  ping_count = rudp_app:env(ping_packet_count),
  is_active = false
}).

-record(state, {
  state = #state_closed{} :: tuple(),
  owner :: pid(),
  socket :: rudp_socket:socket(),
  outgoing_packets = rudp_qos:new(?LEVELS_OF_QOS) :: rudp_qos:qos(),
  sender :: pid(),
  receiver :: pid(),
  timers = rudp_timers:new() :: rudp_timers:timers(),
  packet_counter = rudp_limited_counter:new(?PACKETS_COUNT),
  minimal_packet_counter = 0,
  incomming_packets = rudp_storage:new(0) :: rudp_storage:storage(),
  listener :: rudp_listener:listener(),
  incomming_connection :: rudp_connection:connection_id(),
  outgoing_connection :: rudp_connection:connection_id()
}).

-define(STATE_ATOM(State), element(1, State#state.state)).

-type from() :: {pid(), Tag :: term()}.
-type options() :: maps:maps().

-record(message, {
  data :: binary(),
  from :: from(),
  options :: options()
}).



%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(rudp_listener:listener()) -> { ok, Pid :: pid() }.
start_link(Listener) ->
  gen_server:start_link(?MODULE, [ Listener, self() ], []).

connect(Pid, Address, Port, Options) ->
  connect(Pid, Address, Port, Options, rudp_app:env(connection_timeout)).
connect(Pid, Address, Port, Options, Timeout) ->
  try
    gen_server:call(Pid, { connect, Address, Port, Options, Timeout }, Timeout )
  catch
    exit:{timeout, _ } ->
      { error, connection_timeout }
  end.

close(Pid,Reason) ->
  gen_server:cast(Pid, { close, Reason }).

incomming_packet(Pid, Packet) ->
  gen_server:cast(Pid, { incomming_packet, Packet }).

destroy(Pid) ->
  gen_server:cast(Pid, destroy).

controlling_process(Pid, Owner) when is_pid(Owner) ->
  gen_server:call(Pid, { controlling_process, Owner, self() }).

accept(Pid, IP, ConnectPacket = #connect_packet{ protocol_version = ?PROTOCOL_VERSION }, Timeout) ->
  gen_server:call(Pid, { accept, IP, ConnectPacket }, Timeout);

accept(_Pid, _, #connect_packet{ protocol_version = ProtocolVersion }, _Timeout) ->
  error(bad_protocol_version, [ ProtocolVersion ]).

get_socket(Pid) ->
  gen_server:call(Pid, { get_socket }).

get_state_atom(Pid) ->
  gen_server:call(Pid, { get_state_atom }).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ Listener, Owner]) ->
  {ok, #state{
    owner = Owner,
    listener = Listener
  }}.

handle_call({connect, Address, Port, _Options, Timeout }, From, State) when ?STATE_ATOM(State) =:= state_closed ->
  NewState = create_receiver(State),
  Socket = socket(State),
  IncommingConnection = NewState#state.incomming_connection,
  ok = gen_udp:send(Socket, Address, Port, rudp_packet:connect(IncommingConnection, incomming_port(NewState), ?PROTOCOL_VERSION)),
  NewState0 = NewState#state{
    state = #state_waiting_confirmation_connection_sent{
      from = From,
      address = Address,
      port = Port
    }
  },
  NewState1 = add_timer(waiting_confirmation_connection, Timeout - ?TIMEOUT_DECREASE_STEP, NewState0),
  { noreply, NewState1 };

handle_call({ controlling_process, _, OldOwner }, _From, State) when OldOwner =/= State#state.owner ->
  { reply, { error, is_not_owner }, State };
handle_call({ controlling_process, Owner, _ }, From, State) ->
  link(Owner),
  unlink(State#state.owner),
  gen_server:reply(From, ok),
  case State#state.receiver of
    undefined -> ok;
    Pid when is_pid(Pid) ->
      ok = rudp_receiver:set_owner(Pid, Owner)
  end,
  case State#state.sender of
    undefined -> ok;
    Pid0 when is_pid(Pid0) ->
      ok = rudp_sender:set_owner(Pid0, Owner)
  end,
  { noreply, State#state{
    owner = Owner
  }};

handle_call({ accept, IP, ConnectPacket }, _From, State) when ?STATE_ATOM(State) =:= state_closed ->
  NewState0 = create_receiver(State),
  NewState1 = create_sender(ConnectPacket#connect_packet.connection_id, IP, ConnectPacket#connect_packet.port, NewState0),
  Packet = rudp_packet:connect_ack(NewState1#state.outgoing_connection, NewState1#state.incomming_connection),
  do_send(NewState1, ?LEVELS_OF_QOS, Packet),
  NewState2 = set_state(#state_connected{}, NewState1),
  send_owner({ rudp_connected, do_get_socket(NewState2), IP, ConnectPacket#connect_packet.port}, NewState2),
  { reply, ok, NewState2 };

handle_call({get_socket}, _From, State) ->
  case state(State) of
    #state_connected{} ->
      { reply, { ok, do_get_socket(State) }, State };
    _ ->
      { reply, { error, not_connected }, State }
  end;

handle_call({get_state_atom}, _From, State) ->
  { reply, state_atom(State), State };

handle_call(Request, _From, State) ->
  {reply, { error, { unknown_command_for_state, Request, state_atom(State) }}, State}.

handle_cast({ incomming_packet, Packet }, State) ->
  %% ?INFO("Process packet ~p ~p ", [ Packet, state_atom(State) ]),
  NewState = handle_packet(Packet, state_atom(State), State),
  { noreply, NewState };

handle_cast({ close, Reason},  State) ->
  { noreply, do_close(State, Reason) };

handle_cast(destroy, State) ->
  { stop, normal, State };

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({ timeout, Key}, State) ->
  %% ?INFO("Timer fire ~p ", [ Key ]),
  NewState = delete_timer(Key, State),
  handle_timeout(Key, NewState);

handle_info(_Info, State) ->
  ?INFO("Unknown info message ~p ", [ _Info ]),
  {noreply, State}.

terminate(Reason, State) ->
  ?INFO("Terminate protocol with reason ~p", [ Reason ]),
  do_close(State, Reason),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_timeout(waiting_confirmation_connection, State) when ?STATE_ATOM(State) =:= state_waiting_confirmation_connection_sent ->
  St = state(State),
  From0 = case St of
    #state_waiting_confirmation_connection_sent{ from = From } -> From
  end,
  gen_server:reply(From0, { error, connection_timeout }),
  { noreply, set_state(#state_closed{}, State) };

handle_timeout(ping, State = #state{ state = #state_connected{}}) ->
  case rudp_receiver:get_active_status(State#state.receiver) of
    true ->
      { noreply, add_ping_timer(State)};
    false ->
      St = state(State),
      NewSt = St#state_connected{
        ping_count = St#state_connected.ping_count - 1
      },
      NewState0 = set_state(NewSt, State),
      PingCount = NewSt#state_connected.ping_count,
      NewState1 = case PingCount > 0 of
        true ->
          do_send(NewState0, ?PRIORITY_REALTIME, rudp_packet:ping(State#state.outgoing_connection, PingCount)),
          add_ping_timer(NewState0);
        false ->
          do_close(NewState0, ping_timeout)
      end,
      { noreply, NewState1 }
  end;

handle_timeout(_, State) ->
  { noreply, State }.

handle_packet(#connect_ack_packet{ conn = ReceivedConnectionId}, state_waiting_confirmation_connection_sent, State) ->
  #state_waiting_confirmation_connection_sent{
    address = Address,
    port = Port,
    from = From
  } = state(State),
  NewState1 = create_sender(ReceivedConnectionId, Address, Port, State),
  gen_server:reply(From, ok),
  send_owner({ rudp_connected, do_get_socket(NewState1), Address, Port}, NewState1),
  NewState2 = delete_timer(waiting_confirmation_connection, NewState1 ),
  set_state(#state_connected{}, NewState2);

handle_packet(#close_packet{}, _, State) ->
  do_close(State, closed_by_opponent);

handle_packet(#ping_packet{ count = Count }, state_connected, State) ->
  do_send(State, ?LEVELS_OF_QOS, rudp_packet:ping_ack(State#state.outgoing_connection, Count)),
  State;

handle_packet(#ping_ack_packet{ count = _Count }, state_connected, State) ->
  St = state(State),
  NewSt = St#state_connected{
    ping_count = rudp_app:env(ping_packet_count)
  },
  set_state(NewSt, State);

handle_packet(Packet, St, State) ->
  %% ?ERROR("Bad packet ~p for state ~p", [ Packet, St ]),
  State.

add_timer(Key, Timeout, State) ->
  %% ?INFO("Register timer ~p ~p", [ Key, Timeout ]),
  State#state{
    timers = rudp_timers:add(Key, Timeout, State#state.timers)
  }.

delete_timer(Key, State) ->
  %% ?INFO("Unregister timer ~p ", [ Key ]),
  State#state{
    timers = rudp_timers:delete(Key, State#state.timers)
  }.

set_state(St, State) when element(1, St) =/= element(1, State#state.state) ->
  %% ?INFO("Set state ~p ", [ element(1, St) ]),
  NewState0 = set_state_out(state(State), State),
  NewState1 = NewState0#state{
    state = St
  },
  set_state_in(St, NewState1);
set_state(St, State) when is_tuple(St) ->
  State#state{
    state = St
  }.

set_state_out(#state_connected{}, State) ->
  NewState1 = delete_timer(ping, State),
  NewState1#state{
    socket = undefined
  };
set_state_out(_, State) -> State.

set_state_in(#state_connected{} = St, State) ->
  NewSt = St#state_connected{
    ping_count = rudp_app:env(ping_packet_count)
  },
  NewState = set_state(NewSt, State),
  add_ping_timer(NewState);
set_state_in(_, State) -> State.

state(State) ->
  State#state.state.

state_atom(State) ->
  ?STATE_ATOM(State).

listener(State) ->
  State#state.listener.

socket(State) ->
  Listener = listener(State),
  Listener#listener.socket.

incomming_port(State) ->
  Listener = listener(State),
  Listener#listener.port.

send_owner(Msg, State) ->
  Owner = State#state.owner,
  ?INFO("Send to ~p msg: ~p", [ Owner, Msg ]),
  Owner ! Msg.

create_receiver(State = #state{ receiver = undefined }) ->
  IncommingConnection = rudp_conn_generator:generate(),
  { ok, ReceiverPid } = rudp_receiver:start_link(State#state.owner, IncommingConnection, self()),
  State#state{
    incomming_connection = IncommingConnection,
    receiver = ReceiverPid
  }.

destroy_receiver(State = #state{ receiver = Pid }) when is_pid(Pid) ->
  rudp_receiver:close(Pid),
  State#state{
    incomming_connection = undefined,
    receiver = undefined
  }.

create_sender(ConnId, Address, Port, State = #state{ sender = undefined }) ->
  { ok, Pid } = rudp_sender:start_link(State#state.owner, Address, Port, socket(State), ConnId),
  rudp_receiver:set_sender(State#state.receiver, Pid),
  State#state{
    outgoing_connection = ConnId,
    sender = Pid
  }.

destroy_sender(State = #state{ sender = Pid }) when is_pid(Pid) ->
  rudp_sender:send_packet(Pid, ?LEVELS_OF_QOS, rudp_packet:close(State#state.outgoing_connection)),
  rudp_sender:close(Pid),
  State#state{
    sender = undefined,
    outgoing_connection = undefined
  };
destroy_sender(State) -> State.

do_send(_State = #state{ sender = Pid}, Priority, Binary) when is_pid(Pid) ->
  rudp_sender:send_packet(Pid, Priority, Binary).

do_close(State, Reason) ->
  case state(State) of
    #state_closed{} -> State;
    _ ->
      case state(State) of
        #state_connected{} ->
          send_owner({ rudp_closed, do_get_socket(State), Reason }, State);
        _ -> ok
      end,
      NewState0 = destroy_receiver(State),
      NewState1 = destroy_sender(NewState0),
      set_state(#state_closed{}, NewState1)
  end.

add_ping_timer(State) ->
  add_timer(ping, rudp_app:env(ping_interval), State).

do_get_socket(State) ->
  #rudp_socket{
    protocol = self(),
    sender = State#state.sender,
    receiver = State#state.receiver
  }.

