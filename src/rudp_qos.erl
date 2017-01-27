-module(rudp_qos).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-record(rudp_qos, {
  size = 0 :: pos_integer(),
  levels = {} :: tuple()
}).

-type qos() :: #rudp_qos{}.
-type priority() :: pos_integer().
-export_type([
  qos/0, priority/0
]).

%% API
-export([
  new/1,
  add/3,
  next/1,
  size/1, length/1,
  is_realtime/2, has_realtime/1,
  filter/2, map/2, any/2,
  to_list/1
]).

new(Levels) ->
  #rudp_qos{
    size = Levels,
    levels = list_to_tuple([ queue:new() || _ <- lists:seq(1, Levels) ])
  }.

add(Priority, Term, Qos) ->
  OldQueue = get_queue(Priority, Qos),
  NewQueue = queue:in(Term, OldQueue),
  set_queue(Priority, NewQueue, Qos).

-spec next( qos() ) -> {{ ok, priority(), term()}, qos() } | { undefined, qos() }.
next(Qos) ->
  next(Qos#rudp_qos.size, Qos).

next(0, Qos) -> { undefined, Qos };
next(Priority, Qos) ->
  Queue = get_queue(Priority, Qos),
  case queue:out(Queue) of
    { empty, _ } ->
      next(Priority - 1, Qos);
    {{ value, El }, NewQ } ->
      {{ ok, Priority, El }, set_queue(Priority, NewQ, Qos) }
  end.

is_realtime(Priority, Qos) ->
  Priority >= Qos#rudp_qos.levels.

has_realtime(Qos) ->
  not(queue:is_empty(element(Qos#rudp_qos.size, Qos#rudp_qos.levels))).

size(Qos) ->
  Qos#rudp_qos.size.

filter(Fun, Qos) ->
  Result = lists:map(fun({Q1, Q2}) ->
    NewQ1 = lists:filter(Fun, Q1),
    NewQ2 = lists:filter(Fun, Q2),
    { NewQ1, NewQ2 }
  end, tuple_to_list(Qos#rudp_qos.levels)),
  Qos#rudp_qos{
    levels = list_to_tuple(Result)
  }.

any(Fun, Qos) ->
  lists:any(fun({Q1, Q2}) ->
    lists:any(Fun, Q1) orelse lists:any(Fun, Q2)
  end, tuple_to_list(Qos#rudp_qos.levels)).

map(Fun, Qos) ->
  Result = lists:map(fun({Q1, Q2}) ->
    NewQ1 = lists:map(Fun, Q1),
    NewQ2 = lists:map(Fun, Q2),
    { NewQ1, NewQ2 }
  end, tuple_to_list(Qos#rudp_qos.levels)),
  Qos#rudp_qos{
    levels = list_to_tuple(Result)
  }.

length(Qos) ->
  lists:foldl(fun({Q1,Q2}, S) ->
    S + erlang:length(Q1) + erlang:length(Q2)
  end, 0, tuple_to_list(Qos#rudp_qos.levels)).

to_list(Qos) ->
  lists:foldl(fun(Q, Acc) ->
    queue:to_list(Q) ++ Acc
  end, [], tuple_to_list(Qos#rudp_qos.levels)).

%% INTERNAL

get_queue(Priority, Qos) ->
  element(Priority, Qos#rudp_qos.levels).

set_queue(Priority, Queue, Qos) ->
  Qos#rudp_qos{
    levels = setelement(Priority, Qos#rudp_qos.levels, Queue)
  }.