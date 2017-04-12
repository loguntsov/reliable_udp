-module(rudp_packet).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-include("packet_type.hrl").
-include("logger.hrl").
-include("protocol_const.hrl").

%% API
-export([
  parse/1,
  connection_id/1
]).

-export([
  connect_ack/2,
  close/1,
  ping/2, ping_ack/2,
  data/5, data_ack/2, data_repeat/2, data_udp/2
]).

-type parsed_packet() ::
  #connect_packet{} | #connect_ack_packet{} | #close_packet{} | #ping_packet{} |
  #ping_ack_packet{} | #data_packet{} | #data_ack_packet{} | #data_repeat_packet{} |
  #data_udp_packet{}.

-export_type([ parsed_packet/0 ]).


-spec parse(binary()) -> parsed_packet() | false.
parse(<<?CONNECT:8/unsigned-integer, Port:16/unsigned-integer, ConnectionId:4/binary, ProtocolVersion:64/unsigned-integer, RcvBuffer:64/unsigned-integer>>) ->
  #connect_packet{
    port = Port,
    protocol_version = ProtocolVersion,
    connection_id = ConnectionId,
    rcv_buffer_size = RcvBuffer
  };

parse(<<?CONNECT_ACK:8/unsigned-integer, ConnId:4/binary, MyConnId:4/binary, RcvBuffer:64/unsigned-integer>>) ->
  #connect_ack_packet{
    connection_id = ConnId,
    conn = MyConnId,
    rcv_buffer_size = RcvBuffer
  };

parse(<<?CLOSE:8/unsigned-integer, ConnId:4/binary>>) ->
  #close_packet{
    connection_id = ConnId
  };

parse(<<?PING:8/unsigned-integer, ConnId:4/binary, Count:8/unsigned-integer>>) ->
  #ping_packet{
    connection_id = ConnId,
    count = Count
  };

parse(<<?PING_ACK:8/unsigned-integer, ConnId:4/binary, Count:8/unsigned-integer>>) ->
  #ping_ack_packet{
    connection_id = ConnId,
    count = Count
  };

parse(<<?DATA:8/unsigned-integer, ConnId:4/binary, PacketNumber:32/unsigned-integer, BatchNumber:8/unsigned-integer, 0:6, Start:1, Finish:1, Data/binary>>) ->
 #data_packet{
  connection_id = ConnId,
  packet_number = PacketNumber,
  batch_number = BatchNumber,
  data = binary:copy(Data),
  flags = #{
    start => from_bit(Start),
    finish => from_bit(Finish)
  }
 };

parse(<<?DATA_ACK:8/unsigned-integer, ConnId:4/binary, PacketNumber:32/unsigned-integer>>) ->
  #data_ack_packet{
    connection_id = ConnId,
    packet_number = PacketNumber

  };

parse(<<?DATA_REPEAT:8/unsigned-integer, ConnId:4/binary, PacketNumbers/binary>>) ->
  #data_repeat_packet{
    connection_id = ConnId,
    packet_numbers = parse_packet_numbers(PacketNumbers, [])
  };

parse(<<?DATA_UDP:8/unsigned-integer, ConnId:4/binary, Data/binary>>) ->
  #data_udp_packet{
    connection_id = ConnId,
    data = binary:copy(Data)
  };

parse(Binary) ->
  ?ERROR("Bad packet ~p", [ Binary ]),
  false.

connection_id(Packet) ->
  case Packet of
    <<Type:8/unsigned-integer, ConnectionId:4/binary, _/binary>> when Type =/= ?CONNECT ->
      { ok, Type, ConnectionId };
    <<Type:8/unsigned-integer, _/binary >> -> { undefined, Type }
  end.

-spec build(parsed_packet()) -> binary() | [binary()].
build(#connect_packet{
  port = Port,
  protocol_version = ProtocolVersion,
  connection_id = ConnectionId,
  rcv_buffer_size = RcvBuffer
}) ->
  <<?CONNECT:8/integer, Port:16/unsigned-integer, ConnectionId:4/binary, ProtocolVersion:64/unsigned-integer, RcvBuffer:64/unsigned-integer>>;

build(#connect_ack_packet{
    connection_id = ConnId,
    conn = MyConnId,
    rcv_buffer_size = RcvBuffer
}) ->
  <<(header(?CONNECT_ACK, ConnId))/binary, MyConnId:4/binary, RcvBuffer:64/unsigned-integer>>;

build(#close_packet{
  connection_id = ConnId
}) ->
  <<(header(?CLOSE, ConnId))/binary>>;

build(#ping_packet{
  connection_id = ConnId,
  count = Count
}) ->
  <<(header(?PING, ConnId))/binary, Count:8/unsigned-integer>>;

build(#ping_ack_packet{
  connection_id = ConnId,
  count = Count
}) ->
  <<(header(?PING_ACK, ConnId))/binary, Count:8/unsigned-integer>>;

build(#data_packet{
  connection_id = ConnId,
  packet_number = PacketNumber,
  batch_number = BatchNumber,
  data = Data,
  flags = Flags
}) ->
  Start = to_bit(maps:get(start, Flags, false)),
  Finish = to_bit(maps:get(finish, Flags, false)),
  <<(header(?DATA, ConnId))/binary, PacketNumber:32/unsigned-integer, BatchNumber:8/unsigned-integer, 0:6, Start:1, Finish:1, Data/binary>>;

build(#data_ack_packet{
    connection_id = ConnId,
    packet_number = PacketNumber
}) ->
  <<(header(?DATA_ACK, ConnId))/binary, PacketNumber:32/unsigned-integer>>;

build(#data_repeat_packet{
  connection_id = ConnId,
  packet_numbers = PacketsNumbers
}) ->
  Bodies = data_repeat_loop(PacketsNumbers, []),
  lists:map(fun(Body) ->
    <<(header(?DATA_REPEAT, ConnId))/binary, Body/binary>>
  end, Bodies);

build(#data_udp_packet{
    connection_id = ConnId,
    data = Data
}) ->
  case size(Data) < rudp_udp:max_body_size() of
    true ->
      <<(header(?DATA_UDP, ConnId))/binary, Data/binary>>;
    false ->
      error(big_packet, [ Data ])
  end;

build(Any) -> error(badarg, [ Any ]).

%% BINARY PACKETS GENERATORS

connect_ack(ConnId, ReceivedConnectionId) ->
  build(#connect_ack_packet{
    connection_id = ConnId,
    conn = ReceivedConnectionId
  }).

close(ConnId) ->
  build(#close_packet{
    connection_id = ConnId
  }).

ping(ConnId, Count) ->
  build(#ping_packet{
    connection_id = ConnId,
    count = Count
  }).

ping_ack(ConnId, Count) ->
  build(#ping_ack_packet{
    connection_id = ConnId,
    count = Count
  }).

data(ConnId, PacketNumber, BatchNumber, Flags, Data) ->
  build(#data_packet{
    connection_id = ConnId,
    packet_number = PacketNumber,
    batch_number = BatchNumber,
    data = Data,
    flags = Flags
  }).

data_ack(ConnId, PacketNumber) ->
  build(#data_ack_packet{
      connection_id = ConnId,
      packet_number = PacketNumber
  }).

data_repeat(ConnId, PacketsNumbers) ->
  build(#data_repeat_packet{
    connection_id = ConnId,
    packet_numbers = PacketsNumbers
  }).

data_udp(ConnId, Data) ->
  build(#data_udp_packet{
    connection_id = ConnId,
    data = Data
  }).

%% Internal

data_repeat_binary(List, Acc) when size(Acc) > 1000 -> { List, Acc };
data_repeat_binary([ Number | Tail ], Acc) ->
  B = <<Number:4/unsigned-integer>>,
  NewAcc = <<Acc/binary, B/binary>>,
  data_repeat_binary(Tail, NewAcc).

data_repeat_loop([], Acc) -> lists:reverse(Acc);
data_repeat_loop(List, Acc) ->
  { NewList, Bin } = data_repeat_binary(List, <<>>),
  data_repeat_loop(NewList, [ Bin | Acc ]).

header(Type, ConnId) ->
  <<Type:8/unsigned-integer, ConnId:4/binary>>.

to_bit(true) -> 1;
to_bit(false) -> 0.

from_bit(1) -> true;
from_bit(0) -> false.

parse_packet_numbers(<<>>, Acc) -> Acc;
parse_packet_numbers(<<N:4/unsigned-integer, Binary/binary>>, Acc) ->
  parse_packet_numbers(Binary, [ N | Acc ]).
