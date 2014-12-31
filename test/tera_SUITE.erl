-module(tera_SUITE).
-author("Sungjin Park <jinni.park@gmail.com>").
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() ->
  [{timetrap, {minutes, 1}}].

all() ->
  [{group, default}].

groups() ->
  [{default, [parallel, shuffle], [dummy]}].

dummy(_Config) ->
  ok.
