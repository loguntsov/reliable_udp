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
  protocol_version :: pos_integer(),
  rcv_buffer_size :: pos_integer()
}).

-record(connect_ack_packet, {
  connection_id :: rudp_connection:connection_id(),
  conn :: rudp_connection:connection_id(),
  rcv_buffer_size :: pos_integer()
}).

-record(close_packet, {
  connection_id :: rudp_connection:connection_id()
}).

-record(ping_packet, {
  connection_id :: rudp_connection:connection_id(),
  count :: pos_integer()
}).

-record(ping_ack_packet, {
  connection_id :: rudp_connection:connection_id(),
  count :: pos_integer()
}).

-record(data_packet, {
  connection_id :: rudp_connection:connection_id(),
  packet_number :: pos_integer(),
  batch_number :: pos_integer(),
  flags :: maps:map(),
  data :: binary()
}).

-record(data_ack_packet, {
  connection_id :: rudp_connection:connection_id(),
  packet_number :: pos_integer()
}).

-record(data_repeat_packet, {
  connection_id :: rudp_connection:connection_id(),
  packet_numbers :: [pos_integer()]
}).

-record(data_udp_packet, {
  connection_id :: rudp_connection:connection_id(),
  data :: binary()
}).

