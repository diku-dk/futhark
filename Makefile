# This Makefile mostly serves to abbreviate build commands that are
# unnecessarily obtuse or longwinded.  It depends on the underlying
# build tool (cabal) to actually do anything incrementally.
# Configuration is mostly read from cabal.project.

PREFIX?=$(HOME)/.local

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
	install -d $(PREFIX)/bin/
	install "$$(cabal -v0 list-bin exe:futhark)" $(PREFIX)/bin/futhark

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
	cabal run unit

clean:
	cabal clean
