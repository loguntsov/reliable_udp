-module(rudp_round_robin).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  new/1,
  next/1,
  all/1
]).

-record(round_robin, {
  in  = [] :: [],
  out = [] :: []
}).

-type round_robin() :: #round_robin{}.
-export_type([
  round_robin/0
]).

new([]) -> error(badarg, [[]]);
new(List) when is_list(List) ->
  #round_robin{ in = List, out = [] }.

-spec next(round_robin()) -> { any(), round_robin() }.
next(#round_robin{ in = [A], out = []} = RB) -> { A, RB };
next(#round_robin{ in = [A|Tail], out = Out }) -> { A, #round_robin{ in = Tail, out = [A|Out] }};
next(#round_robin{ in = [], out = Out }) -> next(#round_robin{ in = Out, out = [] }).

all(#round_robin{ in = In, out = Out }) -> In ++ Out.

