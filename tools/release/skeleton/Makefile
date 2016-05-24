#  High-performance purely functional data-parallel array programming on the GPU
# See LICENSE file for copyright and license details.

include config.mk

BINARIES=bin/*

all:
	@echo "This is a precompiled binary distribution of Futhark - no building necessary."
	@echo "But you may want to check out config.mk to ensure that 'make install' installs to the right place."

install:
	@echo Installing executable files to ${PREFIX}/bin
	@mkdir -p ${PREFIX}/bin/
	install bin/* ${PREFIX}/bin/
	@echo Installing manual page to ${MANPREFIX}/man1
	@mkdir -p ${MANPREFIX}/man1/
	@for x in share/man/man1/*.1; do install -D -m 644 $$x ${MANPREFIX}/man1/; done


.PHONY: all install
