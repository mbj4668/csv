PROJECT = csv
PROJECT_DESCRIPTION = Erlang library for parsing csv files
PROJECT_VERSION = 1.1.0

TEST_DEPS = jsone

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

tests ::
	$(MAKE) -C test

clean ::
	$(MAKE) -C test clean
