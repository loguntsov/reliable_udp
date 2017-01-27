-module(rudp_connection).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-include("logger.hrl").

-compile({no_auto_import, [ register/2 ]}).

-type connection_id() :: binary().

-export_type([
  connection_id/0
]).

%% API
-export([
  init/0,
  register/3,
  unregister/2,
  is_exists/2, get_pid/2
]).

-define(ETS, ?MODULE).
-define(COUNTER, counter).

init() ->
  ?ETS = ets:new(?ETS, [ named_table, public, set, { read_concurrency, true }, { write_concurrency, true } ]).

is_exists(Ip, ConnectionId) ->
  ets:member(?ETS, { Ip, ConnectionId}).

register(Ip, ConnId, Pid) ->
  %% ?INFO("Register connection ~p ~p ~p", [ Ip, Pid, ConnId ]),
  case is_exists(Ip, ConnId) of
    true ->  { error, already_exists };
    false ->
      ets:insert(?ETS, {{ Ip, ConnId }, Pid }),
      { ok, ConnId }
  end.

unregister(Ip, ConnId) ->
  %% ?INFO("UnRegister connection ~p ~p ", [ Ip, ConnId ]),
  ets:delete(?ETS, { Ip, ConnId}).

get_pid(Ip, ConnId) ->
  case ets:lookup(?ETS, { Ip, ConnId}) of
    [{_, Pid}] -> { ok, Pid };
    _ -> undefined
  end.