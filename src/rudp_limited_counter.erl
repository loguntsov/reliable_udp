-module(rudp_limited_counter).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  new/1,
  increment/1,
  reset/1,
  value/1, max/1
]).

-record(counter, {
  value = 0 :: pos_integer(),
  max = 1 :: pos_integer()
}).

-type counter() :: #counter{}.
-export_type([
  counter/0
]).

new(Max) ->
  #counter{
    max = Max
  }.

increment(Counter) ->
  limit(Counter#counter{
    value = Counter#counter.value + 1
  }).

reset(Counter) ->
  Counter#counter{
    value = 0
  }.

%% Internal

limit(Counter = #counter{ max = Max, value = Counter }) when Counter > Max ->
  reset(Counter);
limit(Counter) -> Counter.

value(Counter) ->
  Counter#counter.value.

max(Counter) ->
  Counter#counter.max.
