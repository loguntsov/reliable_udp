-module(rudp_listener_registrar).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-include("listener.hrl").

-compile({no_auto_import, [ unregister/1 ]}).

%% API
-export([
  init/0,
  register/1, unregister/1,
  lookup/1
]).

-define(ETS, ?MODULE).

-type something() :: pid() | pos_integer().

init() ->
  ?ETS = ets:new(?ETS, [ named_table, set, public, { write_concurrency, true }, { read_concurrency, true }]).

register(Listener = #listener{}) ->
  ets:insert_new(?ETS, [
    { Listener#listener.port, Listener },
    { Listener#listener.pid, Listener }
  ]).

unregister(Listener = #listener{}) ->
  ets:delete(?ETS, Listener#listener.pid),
  ets:delete(?ETS, Listener#listener.port),
  ok;

unregister(Something) ->
  case lookup(Something) of
    { ok, Listener } ->
      unregister(Listener);
    undefined -> { error, not_found }
  end.

lookup(Something) ->
  case ets:lookup(?ETS, Something) of
    [{ _, Listener }] -> { ok, Listener };
    [] -> undefined
  end.


