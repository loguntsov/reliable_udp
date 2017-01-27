-module(rudp_app).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).

-define(APP, rudp).

-export([
  main/0,
  env/1
]).

main() ->
  { ok, _ } = application:ensure_all_started(rudp),
  %%spawn(fun() -> t:p(), timer:sleep(300000) end),
  ok.

start(_StartType, _StartArgs) ->
  { ok, Pid } = rudp_app_sup:start_link(),
  { ok, Pid }.

stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

env(Key) ->
  { ok, Value } = application:get_env(?APP, Key),
  Value.