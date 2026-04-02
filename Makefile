root :=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
tests :=$(root)/tests
src := $(root)/src
tmp := $(root)/tmp

nrepl:
	guix shell \
	guix \
	guile-next \
	guile-ares-rs \
	-- guile \
	-l nrepl.scm

check:
	if [ -d $(tmp) ]; then rm -rf $(tmp); fi
	guile --no-auto-compile -L $(tests) -L $(src) $(tests)/runner.scm

system-test-datomic-dev:
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services datomic) %test-datomic-dev)'

system-test-datomic-postgres:
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services datomic) %test-datomic-postgres)'
