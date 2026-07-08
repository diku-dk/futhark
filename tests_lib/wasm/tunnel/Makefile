# Example: Tunnel
# Generates the JS/WASM wrapper bundle using the local cabal-built Futhark.
#
# Usage:
#   make gen
#   make serve
#   make clean
#
# Defaults to WASM:
#   futhark wasm --library -o build/tunnel tunnel.fut
#
# You can still override the backend:
#   make gen FUTHARK_BACKEND=webgpu

FUTHARK_BACKEND ?= wasm

PROG := tunnel
FUT  := $(PROG).fut

FUTHARK_BIN_DIR := $(shell cabal -v0 list-bin exe:futhark | xargs dirname)
FUTHARK := PATH="$(FUTHARK_BIN_DIR):$$PATH" futhark

BUILD_DIR := build
OUT := $(BUILD_DIR)/$(PROG)

PORT ?= 8000

.PHONY: all gen clean serve run check print-futhark

all: gen

gen: $(BUILD_DIR)
	$(FUTHARK) $(FUTHARK_BACKEND) --library -o $(OUT) $(FUT)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

check:
	$(FUTHARK) check $(FUT)

clean:
	rm -rf $(BUILD_DIR)

serve: gen
	python3 -m http.server $(PORT)

run: serve

print-futhark:
	@echo "FUTHARK_BIN_DIR=$(FUTHARK_BIN_DIR)"
	@PATH="$(FUTHARK_BIN_DIR):$$PATH" which futhark
	@PATH="$(FUTHARK_BIN_DIR):$$PATH" futhark --version