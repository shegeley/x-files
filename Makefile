root :=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
tests :=$(root)/tests
src := $(root)/src

check:
	guile --no-auto-compile -L $(tests) -L $(src) $(tests)/runner.scm
