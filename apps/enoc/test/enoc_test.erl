-module(enoc_test).
-include_lib("eunit/include/eunit.hrl").

run_test() ->
  Apps = ensure_started(enoc),
  ?assert(lists:keymember(enoc, 1, application:which_applications())),
  ?assertEqual("0.0.1", enoc_time:version()),
  lists:foldl(
    fun(_N, T0) ->
      T = enoc_time:now(),
      ?assert(T >= T0),
      T
    end,
    0,
    lists:seq(1, 1000)),
  ok = ensure_stopped(Apps).

ensure_started(App) ->
  ensure_started(App, []).

ensure_started(App, Apps) ->
  case application:start(App) of
    ok ->
      [App | Apps];
    {error, {not_started, Dep}} ->
      Apps1 = ensure_started(Dep, Apps),
      ensure_started(App, Apps1)
  end.

ensure_stopped([App | Apps]) ->
  ok = application:stop(App),
  ensure_stopped(Apps);
ensure_stopped([]) ->
  ok.
