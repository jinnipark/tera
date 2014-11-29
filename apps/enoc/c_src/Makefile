PRIV_DIR = ../priv
MODULES = $(PRIV_DIR)/enoc_time.so

all: env.mk $(MODULES)

$(PRIV_DIR)/enoc_time.so: enoc_time.o
	@if [ ! -d $(PRIV_DIR) ]; then mkdir $(PRIV_DIR); fi
	gcc -shared -o $@ $^

enoc_time.o: enoc_time.c
	gcc -c -fPIC -I $(ERTS_INCLUDE_DIR) -o $@ $^

clean:
	rm -f $(MODULES)
	rm -f *.o

env.mk:
	@erl -noshell -noinput -eval "file:write_file(\"env.mk\", \
		io_lib:format( \
			\"ERTS_INCLUDE_DIR ?= ~s/erts-~s/include/~n\" \
			\"ERL_INTERFACE_INCLUDE_DIR ?= ~s~n\" \
			\"ERL_INTERFACE_LIB_DIR ?= ~s~n\", \
			[code:root_dir(), erlang:system_info(version), \
			code:lib_dir(erl_interface, include), \
			code:lib_dir(erl_interface, lib)])), \
		erlang:halt()."

-include env.mk
