root :=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
tests :=$(root)/tests
src := $(root)/src
tmp := $(root)/tmp

repl:
	guix shell guile-next guile-ares-rs \
	-- guile \
	-c "((@ (nrepl server) run-nrepl-server) #:port 7888)"

check:
	if [ -d $(tmp) ]; then rm -rf $(tmp); fi
	guile --no-auto-compile -L $(tests) -L $(src) $(tests)/runner.scm
