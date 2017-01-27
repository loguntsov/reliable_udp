-module(rudp_timers).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  new/0,
  add/3, delete/2
]).

-type timers() :: [{ term(), reference}].
-export_type([
  timers/0
]).

new() -> [].

add(Key, Timeout, Timers) ->
  NewTimers0 = delete(Key, Timers),
  Ref = erlang:send_after(Timeout, self(), { timeout, Key }),
  [{Key, Ref} | NewTimers0 ].

delete(Key, Timers) ->
  case proplists:get_value(Key, Timers) of
    undefined -> Timers;
    Ref ->
      erlang:cancel_timer(Ref),
      proplists:delete(Key, Timers)
  end.


