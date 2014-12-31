#include "erl_nif.h"
#include <string.h>
#include <time.h>

static const char* VERSION = "0.0.1";

static ERL_NIF_TERM version_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if (argc != 0)
    return enif_make_badarg(env);
  return enif_make_string_len(env, VERSION, strlen(VERSION), ERL_NIF_LATIN1);
}

static ERL_NIF_TERM now_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  time_t now;
  if (argc != 0)
    return enif_make_badarg(env);
  now = time(NULL);
  return enif_make_int(env, now);
}

static ErlNifFunc nif_funcs[] = {
  {"version", 0, version_nif},
  {"now", 0, now_nif}
};

ERL_NIF_INIT(enoc_time, nif_funcs, NULL, NULL, NULL, NULL)
