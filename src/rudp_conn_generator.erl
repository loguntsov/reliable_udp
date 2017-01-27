-module(rudp_conn_generator).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  start_link/0,
  generate/0
]).

-define(NAME, conn_generator).

start_link() ->
  rudp_uid_generator:start_link(?NAME).

generate() ->
  ConnNumber = rudp_uid_generator:generate(?NAME),
  <<ConnNumber:32/unsigned-integer>>.

