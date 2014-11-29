-module(enoc_time).
-export([version/0, now/0]).

-on_load(init/0).

init() ->
  PrivDir = code:priv_dir(enoc),
  ok = erlang:load_nif(filename:join(PrivDir, atom_to_list(?MODULE)), 0).

version() ->
  exit(nif_library_not_loaded).

now() ->
  exit(nif_library_not_loaded).

