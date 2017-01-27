-module(rudp_udp).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  max_udp_size/0,
  max_body_size/0
]).

-include("protocol_const.hrl").

max_udp_size() ->
  rudp_app:env(udp_packet_size).

max_body_size() ->
  max_udp_size() - ?EMPTY_UDP_SIZE.
