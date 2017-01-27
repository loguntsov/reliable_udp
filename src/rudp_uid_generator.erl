-module(rudp_uid_generator).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behaviour(gen_server).

%% API
-export([
  start_link/1,
  generate/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  id = 0
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [], []).

generate(Name) ->
  gen_server:call(Name, generate).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call(generate, _From, State) ->
  Id = State#state.id + 1,
  { reply, Id, State#state{ id = Id }};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
