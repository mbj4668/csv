DESCRIPTION = Erlang library for parsing csv files

TEST_DEPS = mjson
dep_mjson = git https://github.com/mbj4668/mjson.git

include erl.mk

erl.mk:
	curl -O https://raw.githubusercontent.com/mbj4668/erl.mk/main/$@
