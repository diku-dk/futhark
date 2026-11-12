FUTHARK_BACKEND ?= wasm
FUTHARK ?= futhark
NODE ?= node

BUILD_DIR := build/$(FUTHARK_BACKEND)

ifeq ($(FUTHARK_BACKEND),webgpu)
# Provides WebGPU in Node (see tools/webgpu/node-runner.sh).
NODE_FLAGS := --import ../../tools/webgpu/node-gpu.mjs
else ifeq ($(filter wasm wasm-multicore,$(FUTHARK_BACKEND)),)
$(error Unsupported backend: $(FUTHARK_BACKEND))
endif

TESTS := $(patsubst %.fut,%,$(wildcard *.fut))

.SECONDARY:

.PHONY: test clean

test: $(addprefix do_test_,$(TESTS))

do_test_%: test_%.js harness.js $(BUILD_DIR)/%.mjs
	FUTHARK_BACKEND=$(FUTHARK_BACKEND) BUILD_DIR=$(BUILD_DIR) $(NODE) $(NODE_FLAGS) test_$*.js

$(BUILD_DIR)/%.mjs: %.fut
	mkdir -p $(BUILD_DIR)
	$(FUTHARK) $(FUTHARK_BACKEND) --library -o $(BUILD_DIR)/$* $<

clean:
	rm -rf build
