root :=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
tests :=$(root)/tests
src := $(root)/src
tmp := $(root)/tmp

nrepl:
	guix shell \
	guile-next \
	guile-ares-rs \
	-- guile \
	-l nrepl.scm

check:
	if [ -d $(tmp) ]; then rm -rf $(tmp); fi
	guile --no-auto-compile -L $(tests) -L $(src) $(tests)/runner.scm
