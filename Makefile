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
	cabal haddock \
		--enable-documentation \
		--haddock-html \
		--haddock-options=--show-all \
		--haddock-options=--quickjump \
		--haddock-options=--show-all \
		--haddock-options=--hyperlinked-source

check:
	tools/style-check.sh src unittests

check-commit:
	tools/style-check.sh $$(git diff-index --cached --ignore-submodules=all --name-status HEAD | awk '$$1 != "D" { print $$2 }')

unittest:
	cabal run unit -- --hide-successes

test-oclgrind:
	cabal run -- futhark test tests -c --backend=opencl --exclude=compiled --exclude=no_oclgrind --cache-extension=cache --pass-option=--build-option=-O0 --runner=tools/oclgrindrunner.sh

test-t:
	cabal run -- futhark test tests -t

test-c:
	cabal run -- futhark test tests -c --backend=c --no-tuning

test-multicore:
	cabal run -- futhark test tests -c --backend=multicore --no-tuning

test-interpreter:
	cabal run -- futhark test tests -i

test-structure:
	cabal run -- futhark test tests -s

clean:
	cabal clean
