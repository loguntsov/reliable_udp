-module(rudp_sender).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behaviour(gen_server).

-include("logger.hrl").
-include("priorities.hrl").
-include("protocol_const.hrl").

%% API
-export([
  start_link/5, close/1,
  set_receiver/2,
  async_send_binary/4, sync_send_binary/5,
  send_packet/3,
  change_bandwith/2,
  set_owner/2,
  deliver_confirmation/2, send_data_ack/2,
  get_statistic/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-type packet_type() :: binary | data | sended.

-record(packet, {
  data :: term(),
  type :: packet_type(),
  flags :: #{},
  batch_number :: pos_integer()
}).

-record(sended_packet, {
  packet_number :: pos_integer(),
  attempts = 0 :: pos_integer(),
  binary :: binary(),
  expire_confirmation :: rudp_time:time(),
  started :: rudp_time:time()
}).

-record(state, {
  protocol :: pid(),
  address :: binary(),
  port :: pos_integer(),
  conn_id :: rudp_connection:connection_id(),
  socket :: port(),
  bandwith :: rudp_shaper:shaper(),
  delay = rudp_average:new() :: rudp_average:average(),
  packets = rudp_qos:new(?LEVELS_OF_QOS) :: rudp_qos:qos(),
  batch_counter = rudp_limited_counter:new(255) :: rudp_limited_counter:counter(),
  packet_counter = rudp_limited_counter:new(?MAX_COUNT_PACKETS) :: rudp_limited_counter:counter(),
  timer :: reference() | undefined,
  sended = gb_trees:empty() :: gb_trees:tree(pos_integer(), #sended_packet{}),
  sended_count = 0 :: pos_integer(),
  batch_queue = rudp_qos:new(?LEVELS_OF_QOS) :: rudp_qos:qos(),
  current_batch :: rudp_batch:batch(),
  owner :: pid(),
  check_sended_timer :: reference(),
  bytes = 0 :: pos_integer()
}).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Owner, Address, Port, Socket, ConnId) ->
  { ok, Pid } = gen_server:start_link(?MODULE, [ Owner, self(), Address, Port, Socket, ConnId ], []),
  { ok, Pid }.

set_receiver(Pid, ReceiverPid) ->
  gen_server:cast(Pid, { set_receiver, ReceiverPid }).

set_owner(Pid, Owner) ->
  gen_server:cast(Pid, { set_owner, Owner }).

deliver_confirmation(Pid, PacketNumbers) ->
  gen_server:cast(Pid, { delivery_confirmation, PacketNumbers }).

send_data_ack(Pid, PacketNumber) ->
  gen_server:cast(Pid, { send_data_ack, PacketNumber }).

close(Pid) ->
  gen_server:call(Pid, close).

async_send_binary(Pid, Priority, Type, Binary) ->
  gen_server:cast(Pid, {async_send_binary, Priority, Type, Binary }).

sync_send_binary(Pid, Priority, Type, Binary, Timeout) ->
  gen_server:call(Pid, {sync_send_binary, Priority, Type, Binary }, Timeout).

send_packet(Pid, Priority, Binary) ->
  gen_server:cast(Pid, { send_packet, Priority, Binary }).

change_bandwith(Pid, Bandwith) ->
  gen_server:cast(Pid, { change_bandwith, Bandwith }).

get_statistic(Pid) ->
  gen_server:call(Pid, get_statistic).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ Owner, Protocol, Address, Port, _Socket, ConnId ]) ->
  random:seed(os:timestamp()),
  { ok, Socket } = gen_udp:open(0),
  State = #state{
    protocol = Protocol,
    address = Address,
    port = Port,
    conn_id = ConnId,
    socket = Socket,
    bandwith = rudp_shaper:new(rudp_app:env(bandwith_max)),
    packets = rudp_qos:new(?LEVELS_OF_QOS),
    owner = Owner
  },
  { ok, start_check_timer(State) }.

handle_call({sync_send_binary, Priority, binary, Binary }, From, State) ->
  NewState = add_batch(Priority, Binary, From, State),
  { noreply, NewState };

handle_call(get_statistic, _From, State) ->
  Attempts = max_attempts(State),
  Stat = #{
    bytes => State#state.bytes,
    delay => rudp_average:calc(State#state.delay),
    packets_count => rudp_qos:length(State#state.packets),
    batch_count => rudp_qos:length(State#state.batch_queue),
    bandwith => rudp_shaper:get_max_rate(State#state.bandwith),
    attempts => Attempts,
    sended_count => State#state.sended_count
  },
  { reply, { ok, Stat }, State };

handle_call(_Request, _From, State) ->
  {reply, error, State}.

handle_cast({ send_data_ack, PacketNumber }, State) ->
  Packet = rudp_packet:data_ack(State#state.conn_id, PacketNumber),
  NewState = store_packet(?PRIORITY_HIGH, Packet, State),
  { noreply, NewState };

handle_cast({async_send_binary, Priority, udp, Binary }, State) ->
  Packet = rudp_packet:data_udp(State#state.conn_id, Binary),
  NewState = store_packet(Priority, Packet, State),
  { noreply, NewState };

handle_cast({async_send_binary, Priority, binary, Binary }, State) ->
  NewState = add_batch(Priority, Binary, undefined, State),
  { noreply, NewState };

handle_cast({ delivery_confirmation, PacketNumbers }, State) ->
  Now = rudp_time:now_ms(),
  NewState = lists:foldl(fun(Id, St) ->
    delete_sended(Id, Now, St)
  end, State, PacketNumbers),
  { noreply, NewState };

handle_cast({ set_owner, Owner}, State) ->
  NewState = State#state{
    owner = Owner
  },
  { noreply, NewState };

handle_cast({ change_bandwith, Bandwith }, State) ->
  Shaper = State#state.bandwith,
  OldBandwith = rudp_shaper:get_max_rate(Shaper),
  NewState0 = State#state{
    bandwith = rudp_shaper:set_max_rate(Bandwith, Shaper)
  },
  NewState1 = case OldBandwith < Bandwith andalso State#state.timer =/= undefined of
    true ->
      erlang:cancel_timer(State#state.timer),
      do_send(NewState0#state{ timer = undefined });
    false ->
      NewState0
  end,
  { noreply, NewState1 };

handle_cast({ send_packet, Priority, Packet}, State) ->
  NewState1 = store_packet(Priority, Packet, State),
  { noreply, NewState1 };
handle_cast(close, State) ->
  { stop, normal, State };
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(tick, State) ->
  NewState = do_send(State#state{ timer = undefined }),
  { noreply, NewState };

handle_info(check_sended_packets, State) ->
  NewState = State#state{ check_sended_timer = undefined },
  Now = rudp_time:now_ms(),
  Packets = lists:filter(fun({K, P}) ->
    P#sended_packet.expire_confirmation < Now
  end,gb_trees:to_list(NewState#state.sended)),
  NewState1 = lists:foldl(fun({K, #sended_packet{} = P}, St) ->
    Presented = rudp_qos:any(fun
      (#sended_packet{ packet_number = K0 }) when K0 =:= K -> true;
      (_) -> false
    end, St#state.packets),
    case Presented of
      true -> St;
      false ->
        St0 = St#state{
          packets = rudp_qos:add(?PRIORITY_MIDDLE, P, St#state.packets)
        },
        add_sended(P#sended_packet{ expire_confirmation = undefined }, St0)
    end
  end, NewState, Packets),
  NewState2 = do_send(NewState1),
  Attempts = max_attempts(NewState2),
%%   NewBandwidth = case length(Packets) of
%%     N when N > 0 ->
%%        get_bandwith(NewState2) - N * rudp_app:env(bandwith_step);
%%     0 ->
%%        trunc(get_bandwith(NewState2)*1.1)
%%   end,
  case  Attempts > rudp_app:env(max_attempts_to_send_packet) of
    true ->
      rudp_protocol:close(State#state.protocol, max_attempts_to_send_packet);
    false ->
      ok
  end,
  %% NewState3 = do_change_bandwith(NewBandwidth, NewState2),
  { noreply, start_check_timer(NewState2)};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  ok = gen_udp:close(State#state.socket),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_new_batch(State) ->
  case rudp_qos:next(State#state.batch_queue) of
    {{ ok, _Priority, Batch }, NewBatchQueue} ->
      NewBatchCounter = rudp_limited_counter:increment(State#state.batch_counter),
      BatchNumber = rudp_limited_counter:value(NewBatchCounter),
      TransformedBatch = rudp_batch:transform(BatchNumber, Batch),
      {{ new, TransformedBatch }, State#state{
        batch_counter = NewBatchCounter,
        current_batch = TransformedBatch,
        batch_queue = NewBatchQueue
      }};
    { undefined, _ } ->
      { undefined, State }
  end.

get_current_batch(State) ->
  case State#state.current_batch of
    undefined ->
      get_new_batch(State);
    Batch ->
      {{ current, Batch }, State }
  end.

get_packet_from_batch(State) ->
  case get_current_batch(State) of
    { undefined, NewState } ->
      { undefined, NewState };
    {{ Status, Batch }, NewState } when Status =:= current; Status =:= new ->
      case rudp_batch:get_packet(Batch) of
        undefined ->
          reply(rudp_batch:from(Batch), ok),
          get_packet_from_batch(NewState#state{ current_batch = undefined });
        { ok, Binary, NewBatch } ->
          Flags = #{
            start => Status =:= new,
            finish => rudp_batch:is_empty(NewBatch)
          },
          P = #packet{
            data = Binary,
            type = data,
            flags = Flags,
            batch_number = rudp_limited_counter:value(NewState#state.batch_counter)
          },
          {{ ok, P }, NewState#state{
            current_batch = NewBatch
          }}
      end
  end.

do_send(State) ->
  case State#state.timer =:= undefined orelse rudp_qos:has_realtime(State#state.packets) of
    true ->
      case do_send_0(State) of
        { sended, NewState } ->
          do_send(NewState);
        { undefined, NewState } ->
          NewState
      end;
    false ->
      State
  end.

do_send_0(State) ->
  { Result, NewState0 } = case rudp_qos:next(State#state.packets) of
    {{ ok, _Priority, Packet0 }, NewPackets }  ->
      {{ ok, Packet0 }, State#state{
        packets = NewPackets
      }};
    { undefined, NewPackets } ->
      NewState1 = State#state{ packets = NewPackets },
      case NewState1#state.sended_count < rudp_app:env(send_buffer_size) of
        true ->
          get_packet_from_batch(NewState1);
        false ->
          { undefined, NewState1 }
      end
  end,
  case Result of
    { ok, Packet } ->
      { sended, do_send_now(NewState0, Packet)};
    undefined ->
      { undefined, NewState0 }
  end.

do_send_now(State, Packet = #packet{}) ->
  { SendPacket, NewState } = packet_to_binary(Packet, State),
  do_send_now_0(NewState, SendPacket);

do_send_now(State, Packet = #sended_packet{}) ->
  do_send_now_0(State, Packet).

do_send_now_0(State, Binary) when is_binary(Binary) ->
  ok = gen_udp:send(State#state.socket, State#state.address, State#state.port, Binary),
%%   case Binary of
%%     <<Type:8/unsigned-integer, _/binary>> when Type =:= 8; Type =:= 9 ->
%%       ?INFO("Send packet ~p ", [ Binary ]);
%%     _ -> ok
%%   end,
  { NewShaper, Pause } = rudp_shaper:update(State#state.bandwith, size(Binary) + ?EMPTY_UDP_SIZE),
  NewState = State#state{
    bandwith = NewShaper,
    bytes = State#state.bytes + size(Binary) + ?EMPTY_UDP_SIZE
  },
  %% ?INFO("Timer status ~p", [ NewState#state.timer ]),
  start_timer(Pause, NewState);

do_send_now_0(State, SendedPacket = #sended_packet{}) ->
  Now = rudp_time:now_ms(),
  NewSendedPacket = SendedPacket#sended_packet{
    expire_confirmation = Now + rudp_app:env(delivery_timeout),
    attempts = SendedPacket#sended_packet.attempts + 1
  },
  NewState = add_sended(NewSendedPacket, State),
  do_send_now_0(NewState, SendedPacket#sended_packet.binary).

start_timer(0, State) -> State;
start_timer(Pause, State = #state{ timer = undefined }) when Pause > 0 ->
  Timer = erlang:send_after(Pause, self(), tick),
  State#state{
    timer = Timer
  };
start_timer(_, State) -> State.

store_packet(Priority, Packet, State) when is_binary(Packet) ->
  case rudp_qos:is_realtime(Priority, State#state.packets) of
    false ->
      P = #packet{
        data = Packet,
        type = binary
      },
      NewState0 = State#state{
        packets = rudp_qos:add(Priority, P, State#state.packets)
      },
      do_send(NewState0);
    true ->
      do_send_now(State, Packet)
  end.

add_sended(SendedPacket, State) ->
  PacketNumber = SendedPacket#sended_packet.packet_number,
  case gb_trees:is_defined(PacketNumber, State#state.sended) of
    false ->
      State#state{
        sended = gb_trees:insert(PacketNumber, SendedPacket, State#state.sended),
        sended_count = State#state.sended_count + 1
      };
    true ->
      State#state{
        sended = gb_trees:update(PacketNumber, SendedPacket, State#state.sended)
      }
  end.

delete_sended(PacketNumber, Now, State) ->
  case get_sended(PacketNumber, State) of
    undefined -> State;
    { ok, #sended_packet{ started = Started, expire_confirmation = ExpireConfirmation } } ->
      Delay = Now - Started,
      NewState = State#state{
        delay = rudp_average:add(Delay, State#state.delay),
        sended = gb_trees:delete_any(PacketNumber, State#state.sended),
        sended_count = State#state.sended_count - 1
      },
      case ExpireConfirmation of
        undefined ->
          NewState#state{
            packets = rudp_qos:filter(fun(#sended_packet{ packet_number = N }) when N =:= PacketNumber -> false; (_) -> true end, NewState#state.packets)
          };
        _ -> NewState
      end
  end.

get_sended(PacketNumber, State) ->
  case gb_trees:lookup(PacketNumber, State#state.sended) of
    none -> undefined;
    { value, #sended_packet{} = P } -> { ok, P }
  end.

add_batch(Priority, Binary, From, State) ->
  Batch = rudp_batch:new(undefined, From, Priority, Binary, 5000),
  NewState = State#state{
    batch_queue = rudp_qos:add(Priority, Batch, State#state.batch_queue)
  },
  do_send(NewState).

reply(undefined, _) -> ok;
reply(From, Result) ->
  gen_server:reply(From, Result).

packet_to_binary(Packet = #packet{}, State) ->
  case Packet#packet.type of
    binary ->
      { Packet#packet.data, State };
    Type ->
      NewPacketCounter = rudp_limited_counter:increment(State#state.packet_counter),
      PacketNumber = rudp_limited_counter:value(NewPacketCounter),
      Bin = case Type of
        data ->
          rudp_packet:data(State#state.conn_id, PacketNumber, Packet#packet.batch_number, Packet#packet.flags, Packet#packet.data )
      end,
      SendedPacket = #sended_packet{
        packet_number = PacketNumber,
        binary = Bin,
        started = rudp_time:now_ms()
      },
      { SendedPacket, State#state{
        packet_counter = NewPacketCounter
      }}
   end.

start_check_timer(State = #state{ check_sended_timer = undefined}) ->
  Time = rudp_app:env(delivery_timeout),
  RandomTime = random:uniform(Time div 10),
  T = erlang:send_after(Time + RandomTime , self(), check_sended_packets),
  State#state{
    check_sended_timer = T
  };
start_check_timer(State) -> State.

max_attempts(State) ->
  lists:foldl(fun({ _, #sended_packet{ attempts = A }}, Acc) ->
    max(A, Acc)
  end, 0, gb_trees:to_list(State#state.sended)).

do_change_bandwith(NewBandwith, State) ->
  BandwithMax = rudp_app:env(bandwith_max),
  BandwithMin = rudp_app:env(bandwith_min),
  State#state{
    bandwith = rudp_shaper:set_max_rate(min(BandwithMax, max(BandwithMin, NewBandwith)), State#state.bandwith)
  }.

get_bandwith(State) ->
  rudp_shaper:get_max_rate(State#state.bandwith).

