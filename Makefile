DESCRIPTION = Erlang library for parsing csv files

TEST_DEPS = jsone
dep_jsone = git https://github.com/sile/jsone.git

include erl.mk

erl.mk:
	curl -O https://raw.githubusercontent.com/mbj4668/erl.mk/main/$@
