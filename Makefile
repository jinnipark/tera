PROJECT = tera
DEPS = cowboy gun recon
dep_recon = git https://github.com/ferd/recon.git master

release:: deps apps
	relx

debug:: deps apps
	relx -d

apps:: enoc

enoc::
	cd apps/enoc && make

include erlang.mk