%%% Generic weighted pool, used to pick a value at random with given relative weight.
%%% Provides following operations:
%%% - insert: put a value into the pool
%%% - delete: remove a value from the pool, one with the lowest weight first
%%% - select: get a value in the pool at random
%%%
%%% The implementation is not optimal in terms of select operation's performance but a reasonable
%%% compromise taking complexity, storage and locking into consideration.
%%% Select is apparently O(N) in worst case but expected to show O(log N) or better asymptotic behavior.
-module(gen_weighted_pool).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([new/0]).
-export([insert/2, insert/3]).
-export([delete/2]).
-export([select/1]).

-define(DEFAULT_WEIGHT, 100).

-type pool() :: [{StackedWeight::integer(), Value::any()}].

-spec new() -> pool().
%% Create an empty pool.
new() ->
  [].

-spec insert(Value::any(), pool()) -> pool().
%% Insert a value into pool with default weight.
insert(Value, Pool) ->
  insert(Value, ?DEFAULT_WEIGHT, Pool).

-spec insert(Value::any(), Weight::integer(), pool()) -> pool().
%% Insert a value into pool.
insert(Value, Weight, [{StackedWeight1, Value1}, {StackedWeight2, Value2} | Tail] = Pool) ->
  case Weight < StackedWeight1-StackedWeight2 of
    true ->
      [{StackedWeight1+Weight, Value1} | insert(Value, Weight, [{StackedWeight2, Value2} | Tail])];
    false -> % Inserting a value with the highest weight at the top.
      [{StackedWeight1+Weight, Value} | Pool]
  end;
insert(Value, Weight, [{Weight1, Value1}]) ->
  case Weight < Weight1 of
    true ->
      [{Weight1+Weight, Value1}, {Weight, Value}];
    false ->
      [{Weight+Weight1, Value}, {Weight1, Value1}]
  end;
insert(Value, Weight, []) ->
  [{Weight, Value}].

-spec delete(Value::any(), pool()) -> pool().
%% Delete a value (one with the lowest weight) from pool.
delete(Value, Pool) ->
  % Reverse to scan from back to front.
  delete(Value, lists:reverse(Pool), []).

delete(Value, [{StackedWeight, Value} | Tail], Result) ->
  % Found one to be removed, update all the stacked weights up.
  Weight =
  case catch hd(Result) of
    {'EXIT', _} ->
      StackedWeight;
    {StackedWeight1, _} ->
      StackedWeight-StackedWeight1
  end,
  lists:append(lists:reverse([{S-Weight, V} || {S, V} <- Tail]), Result);
delete(Value, [Head | Tail], Result) ->
  delete(Value, Tail, [Head | Result]);
delete(_Value, [], Result) ->
  Result.

-spec select(Value::pool()) -> any().
%% Select a value at random.
select([]) ->
  error(empty);
select([{_, Value}]) ->
  Value;
select([{StackedWeight, _} | _] = Pool) ->
  case StackedWeight < 1 of
    true ->
      error(invalid);
    false ->
      select(random:uniform(StackedWeight), Pool)
  end.

select(Rand, [{_StackedWeightHigh, Value}, {StackedWeightLow, _} | _]) when Rand > StackedWeightLow ->
  Value; % Hit!
select(Rand, [_, Body | Tail]) ->
  select(Rand, [Body | Tail]); % Pass through
select(_Rand, [{_, Value}]) ->
  Value. % Until there is only one value left.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_REPEAT, 1000000).
-define(TEST_TOLLERANCE, 0.05).

probability_test() ->
  random:seed(os:timestamp()),
  P = new(),
  P1 = insert("alice", 40, P),
  P2 = insert("bruce", 20, P1),
  P3 = insert("charlie", 10, P2),
  P4 = delete("bruce", P3),
  P5 = insert("dave", 30, P4),
  P6 = insert("eddy", 20, P5),
  T = ets:new(gen_weighted_pool_test, []),
  [
    begin
      Value = select(P6),
      case catch ets:update_counter(T, Value, 1) of
        {'EXIT', _} ->
          ets:insert(T, {Value, 1});
        _ ->
          ok
      end
    end
    ||
    _ <- lists:seq(1, ?TEST_REPEAT)
  ],
  ?assert(ets:lookup_element(T, "alice", 2) < ?TEST_REPEAT*(1+?TEST_TOLLERANCE)*0.4),
  ?assert(ets:lookup_element(T, "alice", 2) > ?TEST_REPEAT*(1-?TEST_TOLLERANCE)*0.4),
  ?assert(ets:lookup_element(T, "charlie", 2) < ?TEST_REPEAT*(1+?TEST_TOLLERANCE)*0.1),
  ?assert(ets:lookup_element(T, "charlie", 2) > ?TEST_REPEAT*(1-?TEST_TOLLERANCE)*0.1),
  ?assert(ets:lookup_element(T, "dave", 2) < ?TEST_REPEAT*(1+?TEST_TOLLERANCE)*0.3),
  ?assert(ets:lookup_element(T, "dave", 2) > ?TEST_REPEAT*(1-?TEST_TOLLERANCE)*0.3),
  ?assert(ets:lookup_element(T, "eddy", 2) < ?TEST_REPEAT*(1+?TEST_TOLLERANCE)*0.2),
  ?assert(ets:lookup_element(T, "eddy", 2) > ?TEST_REPEAT*(1-?TEST_TOLLERANCE)*0.2),
  ets:delete(T).

-endif.
