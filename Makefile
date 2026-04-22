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

guix-shell/check:
	if [ -d $(tmp) ]; then rm -rf $(tmp); fi
	guix shell guix guile-next guile-ares-rs -- \
	  guile --no-auto-compile -L $(tests) -L $(src) $(tests)/runner.scm

system/check:
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services datomic) %test-datomic-dev)'
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services datomic) %test-datomic-postgres)'

system-test-datomic-dev:
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services datomic) %test-datomic-dev)'

system-test-datomic-postgres:
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services datomic) %test-datomic-postgres)'

system-test-clickhouse:
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services clickhouse) %test-clickhouse)'

system-test-remark42:
	guix build -L $(src) -L $(tests) --no-grafts --no-offload \
	  -e '(@ (x-files tests services remark42) %test-remark42)'
