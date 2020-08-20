FUTHARK_BACKEND ?= c

ifeq ($(FUTHARK_BACKEND),opencl)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm -lOpenCL
else ifeq ($(FUTHARK_BACKEND),multicore)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm -lpthread
else ifeq ($(FUTHARK_BACKEND),cuda)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm -lcuda -lnvrtc
else
CFLAGS=-O3 -std=c99
LDFLAGS=-lm
endif

.PHONY: test clean

test: test_a test_b test_c test_d test_e
	./test_a
	./test_b
	./test_c
	./test_d
	./test_e

test_%: test_%.c %.o
	gcc -o $@ $^ -Wall -Wextra -pedantic -std=c99 $(LDFLAGS)

%.o: %.fut
	futhark $(FUTHARK_BACKEND) --library $^
	gcc $*.c -c $(CFLAGS)

clean:
	rm -rf test_? ?.c ?.h ?.o
