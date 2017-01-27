-module(rudp_shaper).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-include("logger.hrl").

-export([
  new/1, update/2,
  get_max_rate/1, set_max_rate/2,
  rate/1
]).

-record(shaper, {
  maxrate = 0 :: pos_integer(),
  bytes = 0 :: pos_integer(),
  rate = 0 :: float(),
  fixedtime = 0 :: pos_integer(),
  lasttime = 0 :: integer()
}).

-type shaper() :: #shaper{}.

-export_type([shaper/0]).

new(MaxRate) ->
  #shaper{
    maxrate = MaxRate / 1000,
    rate = rudp_average:new(),
    bytes = 0,
    lasttime = rudp_time:now_ms()
  }.

-spec update(shaper(), integer()) -> {shaper(), integer()}.

update(Shaper, 0) -> { Shaper, 0 };
update(#shaper{ maxrate = MaxRate } = Shaper, Size) ->
  Now = rudp_time:now_ms(),
  DeltaTime = Now - Shaper#shaper.lasttime,
  {NewFixedTime, NewRate } = case Now - Shaper#shaper.fixedtime > 5000 of
    true ->
      { Now, rudp_average:reset(Shaper#shaper.rate) };
    false ->
      { Shaper#shaper.fixedtime, Shaper#shaper.rate }
  end,
  NewState = Shaper#shaper{
    bytes = max(0, Shaper#shaper.bytes + Size - DeltaTime * MaxRate),
    lasttime = Now,
    rate = rudp_average:add(Size, DeltaTime, NewRate),
    fixedtime = NewFixedTime
  },
  Pause = trunc(NewState#shaper.bytes / NewState#shaper.maxrate),
  %% ?INFO("Shaper ~p ~p ~p ~p", [ DeltaTime, Size, NewState, Pause ]),
  { NewState, Pause }.

get_max_rate(Shaper) ->
  Shaper#shaper.maxrate.

set_max_rate(MaxRate, Shaper) ->
  Shaper#shaper{
    maxrate = MaxRate / 1000
  }.

rate(Shaper) ->
  rudp_average:calc(Shaper#shaper.rate).
