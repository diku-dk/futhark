# This Makefile mostly serves to abbreviate build commands that are
# unnecessarily obtuse or longwinded.  It depends on the underlying
# build tool (cabal) to actually do anything incrementally.
# Configuration is mostly read from cabal.project.

PREFIX?=$(HOME)/.local
INSTALLBIN?=$(PREFIX)/bin/futhark

UNAME:=$(shell uname)

# Disable all implicit rules.
.SUFFIXES:

.PHONY: all configure build install docs check check-commit clean

all: build

configure:
	cabal update
	cabal configure

configure-profile:
	cabal configure --enable-profiling --profiling-detail=toplevel-functions

build:
	cabal build

install: build
	install -d $(shell dirname $(INSTALLBIN))
	install "$$(cabal -v0 list-bin exe:futhark)" $(INSTALLBIN)

docs:
	cabal haddock lib:futhark \
		--enable-documentation \
		--haddock-html \
		--haddock-options=--show-all \
		--haddock-options=--quickjump \
		--haddock-options=--show-all \
		--haddock-options=--hyperlinked-source

check:
	tools/style-check.sh src src-testing

check-commit:
	tools/style-check.sh $$(git diff-index --cached --ignore-submodules=all --name-status HEAD | awk '$$1 != "D" { print $$2 }')

unittest:
	cabal run unit -- --hide-successes

clean:
	cabal clean

# All testing targets are below.

# Test that all test programs type check. Much faster than compiling them.
test-t:
	cabal run -- futhark test tests -t

# Run GPU-relevant tests using oclgrind. This includes various adjustments to
# make oclgrind work (it apparently dislikes some LLVM optimisations), and also
# excludes slow workloads because oclgrind is a slow simulator. This is normally
# the best way to test correctness of GPU code generation.
test-oclgrind:
	cabal run -- futhark test tests -c --backend=opencl --exclude=compiled --exclude=no_oclgrind --cache-extension=cache --pass-option=--build-option=-O0 --runner=tools/oclgrindrunner.sh --tuning=tuning_gpu

# Compile all test programs with the 'c' backend and check that they run
# correctly.
test-c:
	cabal run -- futhark test tests -c --backend=c

# Compile all test programs with the 'c' backend, but do not bother running
# them.
test-C:
	cabal run -- futhark test tests -C --backend=c

# Pass all test programs through internalisation, but do not compile or run
# them. This is the most efficient way of testing changes to the compiler
# frontend. Even when working purely on type checker changes, it may be good to
# use this target, as bugs in type inference can cause invalid IR to be
# generated.
test-I:
	cabal run -- futhark test tests -I

# Compile and run all programs with the 'cuda' backend.
test-cuda:
	cabal run -- futhark test tests -c --backend=cuda --tuning=tuning_gpu

# Compile and run all programs with the 'hip' backend.
test-hip:
	cabal run -- futhark test tests -c --backend=hip --tuning=tuning_gpu

# Compile all programs with the 'hip' backend, but do not run them.
test-hip-C:
	cabal run -- futhark test tests -C --backend=hip --tuning=tuning_gpu

# Compile and run all programs with the 'opencl' backend.
test-opencl:
	cabal run -- futhark test tests -c --backend=opencl --tuning=tuning_gpu

# Compile all programs with the 'opencl' backend, but do not run them.
test-opencl-C:
	cabal run -- futhark test tests -C --backend=opencl

# Compile and run all programs with the 'ispc' backend.
test-ispc:
	cabal run -- futhark test -c --backend=ispc tests

# Compile and run all programs with the 'multicore' backend.
test-multicore:
	cabal run -- futhark test tests -c --backend=multicore

# Compile and run all programs with the 'python' backend.
test-python:
	cabal run -- futhark test tests -c --backend=python --exclude=no_python --exclude=compiled

# Run all programs through the interpreter. This excludes slow workloads.
test-interpreter:
	cabal run -- futhark test tests -i

# Run all structure tests. Important after performing work on optimisations,
# particularly on very sensitive ones like flattening or fusion.
test-structure:
	cabal run -- futhark test tests -s

# Run the tests in 'tests_literate'. Note: does not do cabal run.
test-literate:
	cd tests_literate && sh test.sh

# Run the tests in 'tests_property'. Note: does not do cabal run.
test-property:
	cd tests_property && sh test.sh

# Run the tests in 'tests_server'. Note: does not do cabal run.
test-server:
	cd tests_server && sh test.sh
