-module(rudp_app_sup).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  rudp_connection:init(),
  rudp_listener_registrar:init(),

  {ok, {SupFlags, [
    {connection_generator, { rudp_conn_generator, start_link, [] }, permanent, 5000, worker, [ rudp_conn_generator, rudp_uid_generator ] }]
  }}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
