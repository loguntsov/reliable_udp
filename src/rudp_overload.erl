-module(rudp_overload).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  is_overloaded/2,
  protect/3
]).

is_overloaded(Pid, Threshold) ->
  case erlang:process_info(Pid, message_queue_len) of
    { message_queue_len, Len } -> Len > Threshold;
    undefined -> undefined
  end.

protect(Pid, Threshold, Fun) ->
  case is_overloaded(Pid, Threshold) of
    undefined -> { error, unknown };
    true ->
      { error, overloaded };
    false ->
      Fun()
  end.
