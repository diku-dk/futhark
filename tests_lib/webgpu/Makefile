FUTHARK_BACKEND ?= webgpu
TEST ?=

FUTHARK_BIN_DIR := $(shell cabal -v0 list-bin exe:futhark | xargs dirname)
FUTHARK := PATH="$(FUTHARK_BIN_DIR):$$PATH" futhark
NODE ?= $(shell which node)

BUILD_DIR := build
PORT ?= 8000

.PHONY: test run run-headless run-browser clean list check-test check-headless inc array inc-headless array-headless inc-browser array-browser

test: check-test
	$(MAKE) run TEST=$(TEST)

run: run-headless

run-headless: check-headless
	mkdir -p $(BUILD_DIR)/$(TEST)
	$(FUTHARK) $(FUTHARK_BACKEND) --library -o $(BUILD_DIR)/$(TEST)/$(TEST) $(TEST).fut
	$(NODE) test_$(TEST)_headless.js

run-browser: check-test
	mkdir -p $(BUILD_DIR)/$(TEST)
	$(FUTHARK) $(FUTHARK_BACKEND) --library -o $(BUILD_DIR)/$(TEST)/$(TEST) $(TEST).fut
	@echo ""
	@echo "Serving browser test on http://localhost:$(PORT)/test_$(TEST).html"
	@echo "Press Ctrl+C to stop the server."
	python3 -m http.server $(PORT)

inc:
	$(MAKE) run-headless TEST=inc

array:
	$(MAKE) run-headless TEST=array

inc-headless:
	$(MAKE) run-headless TEST=inc

array-headless:
	$(MAKE) run-headless TEST=array

inc-browser:
	$(MAKE) run-browser TEST=inc

array-browser:
	$(MAKE) run-browser TEST=array

check-test:
	@if [ -z "$(TEST)" ]; then \
		echo "Missing TEST name."; \
		echo "Usage:"; \
		echo "  make test TEST=inc"; \
		echo "  make inc-headless"; \
		echo "  make inc-browser"; \
		exit 1; \
	fi
	@if [ ! -f "$(TEST).fut" ]; then \
		echo "Missing Futhark file: $(TEST).fut"; \
		exit 1; \
	fi
	@if [ ! -f "test_$(TEST).html" ]; then \
		echo "Missing browser test file: test_$(TEST).html"; \
		exit 1; \
	fi

check-headless:
	@if [ -z "$(TEST)" ]; then \
		echo "Missing TEST name."; \
		echo "Usage:"; \
		echo "  make inc-headless"; \
		echo "  make array-headless"; \
		exit 1; \
	fi
	@if [ ! -f "$(TEST).fut" ]; then \
		echo "Missing Futhark file: $(TEST).fut"; \
		exit 1; \
	fi
	@if [ ! -f "test_$(TEST)_headless.js" ]; then \
		echo "Missing headless JavaScript test file: test_$(TEST)_headless.js"; \
		exit 1; \
	fi

list:
	@echo "Available tests:"
	@ls *.fut 2>/dev/null | sed 's/\.fut$$//' | sed 's/^/  /'

clean:
	rm -rf $(BUILD_DIR)