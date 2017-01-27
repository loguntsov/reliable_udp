-author("Sergey Loguntsov <loguntsov@gmail.com>").

-define(CONNECT, 1).
-define(CONNECT_ACK, 2).
-define(CLOSE, 3).
-define(CLOSE_ACK, 4).
-define(DATA, 5).
-define(DATA_ACK,6).
-define(DATA_REPEAT, 7).
-define(DATA_UDP, 8).
-define(PING, 9).
-define(PING_ACK, 10).

-record(connect_packet, {
  port :: pos_integer(),
  connection_id :: rudp_connection:connection_id(),
  protocol_version :: pos_integer()
}).

-record(connect_ack_packet, {
  conn :: rudp_connection:connection_id()
}).

-record(close_packet, {}).

-record(ping_packet, {
  count :: pos_integer()
}).

-record(ping_ack_packet, {
  count :: pos_integer()
}).

-record(data_packet, {
  packet_number :: pos_integer(),
  batch_number :: pos_integer(),
  flags :: maps:map(),
  data :: binary()
}).

-record(data_ack_packet, {
  packet_number :: pos_integer()
}).

-record(data_repeat_packet, {
  packet_numbers :: [pos_integer()]
}).

-record(data_udp_packet, {
  data :: binary()
}).

