# This Makefile mostly serves to abbreviate build commands that are
# unnecessarily obtuse or longwinded.  It depends on the underlying
# build tool (cabal) to actually do anything incrementally.
# Configuration is mostly read from cabal.project.

PREFIX?=$(HOME)/.local

.PHONY: all configure build install docs check check-commit clean

all: build

configure:
	cabal update
	cabal configure

build:
	cabal build

install: build
	install -D $$(cabal -v0 list-bin exe:futhark) $(PREFIX)/bin

docs:
	cabal haddock --enable-documentation --haddock-options=--quickjump

check:
	tools/style-check.sh src unittests

check-commit:
	tools/style-check.sh $$(git diff-index --cached --name-status HEAD | awk '$$1 != "D" && /\.l?hsc?$$/ { print $$2 }')

clean:
	cabal clean
