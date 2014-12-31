-module(enoc_time).
-export([version/0, now/0]).
-on_load(init/0).

init() ->
  PrivDir = code:priv_dir(enoc),
  ok = erlang:load_nif(filename:join(PrivDir, "enoc"), 0).

version() ->
  exit(nif_library_not_loaded).

now() ->
  exit(nif_library_not_loaded).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

version_test() ->
  ?assertEqual("0.0.1", version()).

now_test() ->
  lists:foldl(
    fun(_N, T0) ->
      T = ?MODULE:now(),
      ?assert(T >= T0),
      T
    end,
    0,
    lists:seq(1, 1000)).

-endif.
