-module(rudp_storage).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-include("logger.hrl").

%% API
-export([
  new/1,
  add/3,
  set_minimal/2, set_maximal/2,
  is_completed/1, is_presented/2,
  keys/1, to_list/1
]).

-compile(export_all).

-type index() :: pos_integer().
-type el() :: term().

-record(storage, {
  limit :: pos_integer(),
  storage = gb_trees:empty() :: gb_trees:tree(index(), el()),
  min_index = undefined :: pos_integer(),
  max_index = undefined :: pos_integer(),
  size = 0 :: pos_integer()
}).

-type storage() :: #storage{}.
-export_type([
  storage/0
]).

new(Limit) ->
  #storage{
    limit = Limit
  }.

add(Index, Element, Storage) ->
  case is_presented(Index, Storage) of
    true -> Storage;
    false ->
      Storage#storage{
        storage = gb_trees:insert(Index, Element, Storage#storage.storage),
        size = Storage#storage.size + 1
      }
  end.

set_minimal(Min, Storage) ->
  Storage#storage{
    min_index =  Min
  }.

set_maximal(Max, Storage) ->
  Storage#storage{
    max_index = Max
  }.

is_completed(Storage) when is_integer(Storage#storage.min_index), is_integer(Storage#storage.max_index) ->
  %% ?INFO("Storage ~p", [ Storage ]),
  #storage{
    min_index = Min,
    max_index = Max,
    size = Size,
    limit = Limit
  } = Storage,
  case Max < Min of
    false ->
      (( Max - Min ) - Size) =:= -1 ;
    true ->
      ((Max + Limit ) - Min - Size) =:= -1
  end;
is_completed(_Storage) -> false.

is_presented(Index, Storage) ->
  gb_trees:is_defined(Index, Storage#storage.storage).

keys(Storage) ->
  gb_trees:keys(Storage#storage.storage).

to_list(Storage) ->
  List = gb_trees:to_list(Storage#storage.storage),
  #storage{
    limit = Limit
  } = Storage,
  case is_separated(Storage) of
    false ->
      List;
    true ->
      C = Limit div 2,
      lists:sort(lists:map(fun({Key, Value} = Item) ->
        case Key =< C of
          true -> { Key + Limit, Value };
          false -> Item
        end
      end, List))
  end.

%% Internal

is_separated(Storage) ->
  #storage{
    min_index = Min,
    max_index = Max,
    limit = Limit
  } = Storage,
  Max =< Min + Limit div 2.

