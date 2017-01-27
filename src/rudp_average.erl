-module(rudp_average).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  new/0,
  add/2, add/3,
  reset/1,
  calc/1
]).

-record(average, {
  sum = 0 :: float(),
  count = 0 :: pos_integer(),
  old_sum = 0,
  old_count = 0
}).

-type average() :: #average{}.
-export_type([
  average/0
]).

new() ->
  #average{}.

add(Value, Average) ->
  Average#average{
    sum = Average#average.sum + Value,
    count = Average#average.count + 1
  }.

add(Value, Count, Average) ->
  Average#average{
    sum = Average#average.sum + Value,
    count = Average#average.count + Count
  }.

reset(Average) ->
  #average{
    sum = 0,
    count = 0,
    old_sum = Average#average.sum,
    old_count = Average#average.count
  }.

calc(Average) ->
  Count = Average#average.count + Average#average.old_count,
  Sum = Average#average.sum + Average#average.old_sum,
  case Count =:= 0 of
    true -> 0;
    false ->
      Sum / Count
  end.
