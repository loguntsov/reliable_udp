-module(rudp_receiver).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-include("logger.hrl").
-include("packet_type.hrl").
-include("protocol_const.hrl").

-behaviour(gen_server).

%% API
-export([
  start_link/3,
  process_packet/2,
  close/1,
  set_ip/2, set_sender/2, set_owner/2,
  get_active_status/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(missed, {
  time :: rudp_time:time()
}).

-record(state, {
  ip :: undefined | inet:ip_address(),
  conn_id :: rudp_connection:connection_id(),
  socket :: rudp_socket:socket(),
  sender :: pid(),
  protocol :: pid(),
  owner :: pid(),
  last_packet_number = 0 :: pos_integer(),
  current_batch_number = 0 :: pos_integer(),
  storage = [] :: proplists:proplists(BatchNumber :: pos_integer(), rudp_storage:storage()),
  missed_packets = gb_trees:empty() :: gb_trees:tree(PacketNUmber :: pos_integer(), #missed{}),
  is_active = true :: boolean()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Owner, ConnId, ProtocolPid ) ->
  { ok, Pid } = gen_server:start_link(?MODULE, [ Owner, ConnId, ProtocolPid ], []),
  { ok, Pid }.

close(Pid) ->
  gen_server:cast(Pid, close).

set_sender(Pid, SenderPid) ->
  gen_server:cast(Pid, { set_sender, SenderPid }).

set_owner(Pid, Owner) ->
  gen_server:cast(Pid, { set_owner, Owner }).

set_ip(Pid, Ip) ->
  gen_server:cast(Pid, { set_ip, Ip }).

process_packet(Pid, Packet) ->
  rudp_overload:protect(Pid, 100, fun() ->
    gen_server:cast(Pid, { process_packet, Packet })
  end).

get_active_status(Pid) ->
  gen_server:call(Pid, get_active_status).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Owner, ConnId, ProtocolPid ]) ->
  rudp_connection:register(undefined, ConnId, self()),
  {ok, #state{
    ip = undefined,
    conn_id = ConnId,
    protocol = ProtocolPid,
    socket = undefined,
    sender = undefined,
    owner = Owner
  }}.

handle_call(get_active_status, _From, State) ->
  { reply, State#state.is_active, State#state{
    is_active = false
  }};

handle_call(_Request, _From, State) ->
  {reply, error, State}.

handle_cast({ process_packet, Packet }, State) ->
  Parsed = rudp_packet:parse(Packet),
  NewState0 = State#state{
    is_active = true
  },
  %% ?INFO("Process packet ~p", [ Parsed ]),
  NewState1 = case Parsed of
    false -> NewState0;
    DataPacket = #data_packet{ packet_number = PacketNumber } ->
      NewSt0 = delete_missed(PacketNumber, NewState0),
      handle_packet(DataPacket, NewSt0);
    DataAckPacket = #data_ack_packet{} ->
      rudp_sender:deliver_confirmation(State#state.sender, [ DataAckPacket#data_ack_packet.packet_number ]),
      NewState0;
    ParsedPacket ->
      rudp_protocol:incomming_packet(State#state.protocol, ParsedPacket),
      NewState0
  end,
  { noreply, NewState1, hibernate };

handle_cast({ set_sender, SenderPid }, State) ->
  link(SenderPid),
  { ok, Socket } = rudp_protocol:get_socket(State#state.protocol),
  { noreply, State#state{
    sender = SenderPid,
    socket = Socket
  }};

handle_cast({ set_ip, Ip}, State) ->
  NewState = case State#state.ip of
    undefined ->
      rudp_connection:register(Ip, State#state.conn_id, self()),
      rudp_connection:unregister(undefined, State#state.conn_id),
      State#state{
        ip = Ip
      };
    _ -> State
  end,
  { noreply, NewState };

handle_cast({set_owner, Owner }, State) ->
  NewState = State#state{
    owner = Owner
  },
  { noreply, NewState };

handle_cast(stop, State) ->
  { stop, normal, State };

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  rudp_connection:unregister(State#state.ip, State#state.conn_id),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_missed(PacketsIds, Now, State) ->
  M = #missed{
    time = Now
  },
  NewMissed0 = lists:foldl(fun(Id, Acc) ->
    gb_trees:insert(Id, M, Acc)
  end, State#state.missed_packets, PacketsIds),
  NewMissed1 = case length(PacketsIds) > 10 of
    true ->
      gb_trees:balance(NewMissed0);
    false ->
      NewMissed0
  end,
  State#state{
    missed_packets = NewMissed1
  }.

delete_missed(PacketId, State) ->
  State#state{
    missed_packets = gb_trees:delete_any(PacketId, State#state.missed_packets)
  }.

presented_in_missed(PacketId, State) ->
  gb_trees:is_defined(PacketId, State#state.missed_packets).

get_batch(BatchId, State) ->
  case proplists:get_value(BatchId, State#state.storage, undefined) of
    undefined ->
      rudp_storage:new(?MAX_COUNT_BATCHES);
    Batch -> Batch
  end.

set_batch(BatchId, Storage, State) ->
  NewState =  delete_batch(BatchId, State),
  NewState#state{
    storage = [{BatchId, Storage} | NewState#state.storage ]
  }.

delete_batch(BatchId, State) ->
  State#state{
    storage = proplists:delete(BatchId, State#state.storage)
  }.

handle_packet(Packet = #data_packet{}, State) ->
  #data_packet{
    packet_number = PacketId,
    flags = PacketFlags,
    batch_number = PacketBatchId
  } = Packet,
  rudp_sender:send_data_ack(State#state.sender, PacketId),
  CurrentNumber = State#state.last_packet_number,
  NewState0 = if
    PacketId < CurrentNumber + ( ?MAX_COUNT_PACKETS / 2 ) ->
      State#state{
        last_packet_number = PacketId
      };
    CurrentNumber + 1 < PacketId ->
      Now = rudp_time:now_ms(),
      MissedIds = lists:seq(CurrentNumber + 1, PacketId - 1),
      add_missed(MissedIds, Now, State);
    true -> State
  end,
  NewState1 = NewState0#state{
    last_packet_number = max(CurrentNumber, PacketId)
  },
  Batch = get_batch(PacketBatchId, NewState1),
  NewBatch0 = rudp_storage:add(Packet#data_packet.packet_number, Packet#data_packet.data, Batch),
  NewBatch1 = case maps:get(start, PacketFlags, false) of
    false -> NewBatch0;
    true -> rudp_storage:set_minimal(PacketId, NewBatch0)
  end,
  NewBatch2 = case maps:get(finish, PacketFlags, false) of
    false -> NewBatch1;
    true -> rudp_storage:set_maximal(PacketId, NewBatch1)
  end,
  NewState2 = case rudp_storage:is_completed(NewBatch2) of
    false ->
      set_batch(PacketBatchId, NewBatch2, NewState1);
    true ->
      Binary = iolist_to_binary([ B || { _, B } <- rudp_storage:to_list(NewBatch2)]),
      Owner = NewState1#state.owner,
      %% ?INFO("Received binary ~p ~p", [ Binary, Owner ]),
      Owner ! { rudp_received, NewState1#state.socket, Binary },
      delete_batch(PacketBatchId, NewState1)
  end,
  NewState2.


