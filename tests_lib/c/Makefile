FUTHARK_BACKEND ?= c

ifeq ($(FUTHARK_BACKEND),opencl)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm -lOpenCL
else ifeq ($(FUTHARK_BACKEND),multicore)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm
else ifeq ($(FUTHARK_BACKEND),ispc)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm
ISPCFLAGS=--woff --pic --addressing=64
else ifeq ($(FUTHARK_BACKEND),cuda)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm -lcuda -lcudart -lnvrtc
else ifeq ($(FUTHARK_BACKEND),hip)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm -lamdhip64 -lhiprtc
else
CFLAGS=-O3 -std=c99
LDFLAGS=-lm
endif

.SECONDARY:

.PHONY: test clean

test: $(patsubst %.fut, do_test_%, $(wildcard *.fut))

do_test_%: test_%
	check-jsonschema -q --schemafile ../../docs/manifest.schema.json $*.json
	./test_$*

test_%: test_%.c %.o
  ifeq ($(FUTHARK_BACKEND),ispc)
	gcc -o $@ $^ $(patsubst test_%, %_ispc.o, $@) -Wall -Wextra -pedantic -std=c99 $(LDFLAGS) -lpthread
  else
	gcc -o $@ $^ -Wall -Wextra -pedantic -std=c99 $(LDFLAGS) -lpthread
  endif

%.o: %.c
  ifeq ($(FUTHARK_BACKEND),ispc)
	ispc -o $*_ispc.o $*.kernels.ispc $(ISPCFLAGS)
  endif
	gcc $*.c -c $(CFLAGS)

%.c: %.fut
	futhark $(FUTHARK_BACKEND) --library $^

clean:
	rm -rf $(patsubst %.c, %, $(wildcard test_*.c)) *.h *.o *.ispc $(patsubst %.fut, %.c, $(wildcard *.fut))
