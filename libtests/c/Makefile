FUTHARK_BACKEND ?= c

ifeq ($(FUTHARK_BACKEND),opencl)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm -lOpenCL
else ifeq ($(FUTHARK_BACKEND),multicore)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm
else ifeq ($(FUTHARK_BACKEND),cuda)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm -lcuda -lcudart -lnvrtc
else
CFLAGS=-O3 -std=c99
LDFLAGS=-lm
endif

.PHONY: test clean

test: do_test_a do_test_b do_test_c do_test_d do_test_e do_test_f

do_test_%: test_%
	./test_$*

test_%: test_%.c %.o
	gcc -o $@ $^ -Wall -Wextra -pedantic -std=c99 $(LDFLAGS) -lpthread

%.o: %.fut
	futhark $(FUTHARK_BACKEND) --library $^
	gcc $*.c -c $(CFLAGS)

clean:
	rm -rf test_? ?.c ?.h ?.o
