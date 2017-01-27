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
  connect/3,
  connect_ack/2,
  close/1,
  ping/2, ping_ack/2,
  data/5, data_ack/2, data_repeat/2, data_udp/2
]).

parse(<<?CONNECT:8/unsigned-integer, Port:16/unsigned-integer, ConnectionId:4/binary, ProtocolVersion:64/unsigned-integer>>) ->
  #connect_packet{
    port = Port,
    protocol_version = ProtocolVersion,
    connection_id = ConnectionId
  };

parse(<<?CONNECT_ACK:8/unsigned-integer, _:4/binary, MyConnId:4/binary>>) ->
  #connect_ack_packet{
    conn = MyConnId
  };

parse(<<?CLOSE:8/unsigned-integer, _:4/binary>>) ->
  #close_packet{};

parse(<<?PING:8/unsigned-integer, _:4/binary, Count:8/unsigned-integer>>) ->
  #ping_packet{
    count = Count
  };

parse(<<?PING_ACK:8/unsigned-integer, _:4/binary, Count:8/unsigned-integer>>) ->
  #ping_ack_packet{
    count = Count
  };

parse(<<?DATA:8/unsigned-integer, _:4/binary, PacketNumber:32/unsigned-integer, BatchNumber:8/unsigned-integer, 0:6, Start:1, Finish:1, Data/binary>>) ->
 #data_packet{
  packet_number = PacketNumber,
  batch_number = BatchNumber,
  data = binary:copy(Data),
  flags = #{
    start => from_bit(Start),
    finish => from_bit(Finish)
  }
 };

parse(<<?DATA_ACK:8/unsigned-integer, _:4/binary, PacketNumber:32/unsigned-integer>>) ->
  #data_ack_packet{
    packet_number = PacketNumber
  };

parse(<<?DATA_REPEAT:8/unsigned-integer, _:4/binary, PacketNumbers/binary>>) ->
  #data_repeat_packet{
    packet_numbers = parse_packet_numbers(PacketNumbers, [])
  };

parse(<<?DATA_UDP:8/unsigned-integer, _:4/binary, Data/binary>>) ->
  #data_udp_packet{
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

%% BINARY PACKETS GENERATORS

connect(ConnId, Port, ProtocolVersion) ->
  <<?CONNECT:8/integer, Port:16/unsigned-integer, ConnId:4/binary, ProtocolVersion:64/unsigned-integer>>.

connect_ack(ConnId, ReceivedConnectionId) ->
  <<(header(?CONNECT_ACK, ConnId))/binary, ReceivedConnectionId:4/binary>>.

close(ConnId) ->
  <<(header(?CLOSE, ConnId))/binary>>.

ping(ConnId, Count) ->
  <<(header(?PING, ConnId))/binary, Count:8/unsigned-integer>>.

ping_ack(ConnId, Count) ->
  <<(header(?PING_ACK, ConnId))/binary, Count:8/unsigned-integer>>.

data(ConnId, PacketNumber, BatchNumber, Flags, Data) ->
  Start = to_bit(maps:get(start, Flags, false)),
  Finish = to_bit(maps:get(finish, Flags, false)),
  <<(header(?DATA, ConnId))/binary, PacketNumber:32/unsigned-integer, BatchNumber:8/unsigned-integer, 0:6, Start:1, Finish:1, Data/binary>>.

data_ack(ConnId, PacketNumber) ->
  <<(header(?DATA_ACK, ConnId))/binary, PacketNumber:32/unsigned-integer>>.

data_repeat(ConnId, PacketsNumbers) ->
  Bodies = data_repeat_loop(PacketsNumbers, []),
  lists:map(fun(Body) ->
    <<(header(?DATA_REPEAT, ConnId))/binary, Body/binary>>
  end, Bodies).

data_udp(ConnId, Data) ->
  case size(Data) < rudp_udp:max_body_size() of
    true ->
      <<(header(?DATA_UDP, ConnId))/binary, Data/binary>>;
    false ->
      error(big_packet, [ Data ])
  end.

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
