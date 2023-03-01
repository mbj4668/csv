SHELL := /bin/bash

.PHONY: test test1 test2 test3
test: test1 test2 test3

test1: csv-test-data
	@set -o pipefail;							\
	for f in csv-test-data/csv/*.csv; do					\
	    echo -n "$$f ...";							\
	    b=`basename $$f .csv`;						\
	    t=$$(tempfile) || exit;						\
	    trap "rm -f -- '$$t'" EXIT;						\
	    ./csv2json $$f 2> /dev/null | jq . > $$t;				\
	    r=$$?;								\
	    if [[ $$b = bad-header-* || $$b = header-* ]]; then 		\
		echo "SKIP";							\
	    elif [[ $$r -ne 0 && $$b = bad-* ]]; then				\
	        echo "ok";							\
	    elif [[ $$b = bad-* ]]; then					\
	        echo "nok - should have failed";				\
	        exit 1;								\
	    elif cat csv-test-data/json/$$b.json | jq . | diff $$t -; then	\
	        echo "ok";							\
	    else								\
	        exit 1;								\
	    fi;									\
	    rm -f -- $$t;							\
	done

test2: comments.csv
	@set -o pipefail;							\
	echo -n "$< ...";							\
	t=$$(tempfile) || exit;							\
	trap "rm -f -- '$$t'" EXIT;						\
	./csv2json --no-strict $< | jq . > $$t;					\
	if cat comments.json | diff $$t -; then					\
	    echo "ok";								\
	else									\
	    exit 1;								\
	fi;									\
	rm -f -- $$t;

test3: semi-colon.csv
	@set -o pipefail;							\
	echo -n "$< ...";							\
	t=$$(tempfile) || exit;							\
	trap "rm -f -- '$$t'" EXIT;						\
	./csv2json --separator ";" $< | jq . > $$t;				\
	if cat semi-colon.json | diff $$t -; then				\
	    echo "ok";								\
	else									\
	    exit 1;								\
	fi;									\
	rm -f -- $$t;

csv-test-data:
	git clone --depth=1 https://github.com/sineemore/csv-test-data.git

clean:
	rm -rf csv-test-data
