.PHONY: all deps test run clean concisetest
.DEFAULT_GOAL := test

# Discover all .fut tests recursively under tests/
TESTS := $(sort $(shell find tests -path tests/lib -prune -o -type f -name '*.fut' -print))

DEPS_STAMP := tests/.futhark-deps.stamp
DEPS_INPUTS := $(wildcard tests/futhark.pkg tests/futhark.lock tests/futhark.toml tests/futhark.json)

all: test

deps: $(DEPS_STAMP)

$(DEPS_STAMP): $(DEPS_INPUTS)
	cd tests && futhark pkg sync
	@touch $@

run: deps
	@test -n "$(file)" || (echo "Usage: make run file=tests/foo.fut" >&2; exit 2)
	cabal run futhark-quickcheck -- "$(file)"

test: deps
	@echo "Building futhark-quickcheck..."
	@cabal build futhark-quickcheck
	@echo "Running tests..."
	@set -e; \
	files="$(if $(ONLY),$(ONLY),$(TESTS))"; \
	cabal run futhark-quickcheck -- $$files

concisetest: deps
	@echo "Building futhark-quickcheck..."
	@cabal build futhark-quickcheck
	@echo "Running tests (concise output)..."
	@set -e; \
	files="$(if $(ONLY),$(ONLY),$(TESTS))"; \
	cabal run futhark-quickcheck -- --concise $$files

clean:
	cabal clean
	rm -rf tests/lib
	rm -f $(DEPS_STAMP)
	cd tests && rm -f *.o *.hi *.c *.so test_int test_sort_dedup
	@# Sweep compiled artifacts and binaries under tests/
	@find tests -type f \( \
		-name '*.o' -o -name '*.hi' -o -name '*.c' -o -name '*.so' -o -name '*.a' -o -name '*.dylib' -o -name '*.dll' -o \
		-name '*.exe' -o -name '*.out' \
	\) -print -delete
	@# Remove any remaining executables produced during tests (heuristic: executable bit set)
	@find tests -type f -perm -111 \
		! -name '*.fut' ! -name '*.txt' ! -name '*.md' ! -name '*.json' ! -name '*.toml' ! -name '*.lock' ! -name '*.pkg' \
		-print -delete