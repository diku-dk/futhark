FUTHARK_BACKEND ?= python

.PHONY: test clean

test: a.py
	./test_a

test_%: %.py

%.py: %.fut
	futhark $(FUTHARK_BACKEND) --library $^

clean:
	rm -rf test_? ?.c ?.h
