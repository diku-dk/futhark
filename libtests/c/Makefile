FUTHARK_BACKEND ?= c

.PHONY: test clean

test: test_a test_b
	./test_a
	./test_b

test_%: test_%.c %.o
	gcc -o $@ $^ -Wall -Wextra -pedantic -std=c99

%.o: %.fut
	futhark $(FUTHARK_BACKEND) --library $^
	gcc $*.c -c
