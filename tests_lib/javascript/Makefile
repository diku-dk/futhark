FUTHARK_BACKEND ?= wasm
FUTHARK ?= futhark
NODE ?= node

BUILD_DIR := build/$(FUTHARK_BACKEND)

# The file that loadLibrary() loads.
ifneq ($(filter wasm wasm-multicore,$(FUTHARK_BACKEND)),)
LIB_EXT := mjs
else
$(error Unsupported backend: $(FUTHARK_BACKEND))
endif

TESTS := $(patsubst %.fut,%,$(wildcard *.fut))

.SECONDARY:

.PHONY: test clean

test: $(addprefix do_test_,$(TESTS))

do_test_%: test_%.js harness.js $(BUILD_DIR)/%.$(LIB_EXT)
	FUTHARK_BACKEND=$(FUTHARK_BACKEND) BUILD_DIR=$(BUILD_DIR) $(NODE) test_$*.js

$(BUILD_DIR)/%.$(LIB_EXT): %.fut
	mkdir -p $(BUILD_DIR)
	$(FUTHARK) $(FUTHARK_BACKEND) --library -o $(BUILD_DIR)/$* $<

clean:
	rm -rf build
