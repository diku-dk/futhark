FUTHARK_BACKEND ?= python

.PHONY: test clean

test: do_test_a do_test_g

do_test_%: test_% %.py
	./test_$*

test_%: %.py

%.py: %.fut
	futhark $(FUTHARK_BACKEND) --library $^

clean:
	rm -rf test_? ?.c ?.h
