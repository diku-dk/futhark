FUTHARK_BACKEND ?= wasm
TEST ?=

FUTHARK_BIN_DIR := $(shell cabal -v0 list-bin exe:futhark | xargs dirname)
FUTHARK := PATH="$(FUTHARK_BIN_DIR):$$PATH" futhark
NODE ?= $(shell which node)

BUILD_DIR := build

.PHONY: test run clean list check-test inc array

test: check-test
	$(MAKE) run TEST=$(TEST)

run: check-test
	mkdir -p $(BUILD_DIR)/$(TEST)
	$(FUTHARK) $(FUTHARK_BACKEND) --library -o $(BUILD_DIR)/$(TEST)/$(TEST) $(TEST).fut
	$(NODE) test_$(TEST).js

inc:
	$(MAKE) run TEST=inc

array:
	$(MAKE) run TEST=array

check-test:
	@if [ -z "$(TEST)" ]; then \
		echo "Missing TEST name."; \
		echo "Usage:"; \
		echo "  make test TEST=inc"; \
		echo "  make run TEST=inc"; \
		echo "  make inc"; \
		exit 1; \
	fi
	@if [ ! -f "$(TEST).fut" ]; then \
		echo "Missing Futhark file: $(TEST).fut"; \
		exit 1; \
	fi
	@if [ ! -f "test_$(TEST).js" ]; then \
		echo "Missing JavaScript test file: test_$(TEST).js"; \
		exit 1; \
	fi

list:
	@echo "Available tests:"
	@ls *.fut 2>/dev/null | sed 's/\.fut$$//' | sed 's/^/  /'

clean:
	rm -rf $(BUILD_DIR)