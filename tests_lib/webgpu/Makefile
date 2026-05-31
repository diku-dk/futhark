FUTHARK_BACKEND ?= webgpu
TEST ?=

FUTHARK_BIN_DIR := $(shell cabal -v0 list-bin exe:futhark | xargs dirname)
FUTHARK := PATH="$(FUTHARK_BIN_DIR):$$PATH" futhark
NODE ?= $(shell which node)
NPM ?= $(shell which npm)
NPX ?= $(shell which npx)

BUILD_DIR := build

.PHONY: test run clean list check-test setup-playwright inc array

test: check-test
	$(MAKE) run TEST=$(TEST)

run: check-test
	mkdir -p $(BUILD_DIR)/$(TEST)
	$(FUTHARK) $(FUTHARK_BACKEND) --library -o $(BUILD_DIR)/$(TEST)/$(TEST) $(TEST).fut
	$(NODE) test_$(TEST).js

serve: gen
	python3 -m http.server 8000

setup-playwright:
	@if [ ! -f package.json ]; then \
		npm init -y; \
	fi
	$(NPM) install playwright
	$(NPX) playwright install chromium
# you might need to run `npx playwright install-deps` if you haven't done so before

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

print-futhark:
	@echo "FUTHARK_BIN_DIR=$(FUTHARK_BIN_DIR)"
	@PATH="$(FUTHARK_BIN_DIR):$$PATH" which futhark
	@PATH="$(FUTHARK_BIN_DIR):$$PATH" futhark --version

clean:
	rm -rf $(BUILD_DIR)