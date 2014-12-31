PROJECT = tera
APPS = $(wildcard apps/*)
LIBS = $(wildcard libs/*)
DEPS += recon
dep_recon = git https://github.com/ferd/recon.git master

.PHONY: all apps libs deps clean tests

ALL_APPS_DIR = $(addprefix $(CURDIR)/,$(APPS))
ALL_LIBS_DIR = $(addprefix $(CURDIR)/,$(LIBS))

apps:: deps libs
	$(gen_verbose) $(foreach app,$(ALL_APPS_DIR),$(call make_2,$(app),app))

libs:: deps
	$(gen_verbose) $(foreach lib,$(ALL_LIBS_DIR),$(call make_2,$(lib),app))

deps::
	$(gen_verbose) $(foreach lib,$(ALL_LIBS_DIR),$(call make_2,$(lib),deps))
	$(gen_verbose) $(foreach app,$(ALL_APPS_DIR),$(call make_2,$(app),deps))

clean:: clean-apps clean-libs clean-deps clean-test

clean-apps::
	$(gen_verbose) $(foreach app,$(ALL_APPS_DIR),$(call make_2,$(app),clean))

clean-libs::
	$(gen_verbose) $(foreach lib,$(ALL_LIBS_DIR),$(call make_2,$(lib),clean))

clean-deps::
	$(gen_verbose) $(foreach dep,$(addprefix $(CURDIR)/,$(wildcard deps/*)),$(call make_2,$(dep),clean))

test:: test-deps test-libs test-apps
test:: ERLC_OPTS = $(TEST_ERLC_OPTS)
test:: clean deps libs apps build-ct-suites
	@if [ -d "test" ] && [ -n "$(CT_SUITES)" ]; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) ; \
	fi

test-deps::
ifndef skip_deps
	$(gen_verbose) $(foreach dep,$(addprefix $(CURDIR)/,$(wildcard deps/*)),$(call make_2,$(dep),tests))
endif

test-libs::
	$(gen_verbose) $(foreach lib,$(ALL_LIBS_DIR),$(call make_2,$(lib),tests))

test-apps::
	$(gen_verbose) $(foreach app,$(ALL_APPS_DIR),$(call make_2,$(app),tests))

clean-test::
	$(gen_verbose) rm -f test/*.beam

define make_2
	if [ -f "$(1)/makefile" ] || [ -f "$(1)/Makefile" ]; then \
		$(MAKE) -S -w -C $(1) $(2); \
	else \
		echo "include $(CURDIR)/erlang.mk" | ERLC_OPTS=+debug_info $(MAKE) -S -w -f - -C $(1) $(2); \
	fi;
endef

include erlang.mk
