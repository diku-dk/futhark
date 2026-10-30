# Example: Tunnel (Futhark WebGPU)
# Generates the JS/WASM wrapper bundle using:
#   futhark webgpu --library -o build/tunnel tunnel.fut

PROG  := tunnel
FUT   := $(PROG).fut
BUILD := build
OUT   := $(BUILD)/$(PROG)

GEN := $(OUT).js \
       $(OUT).wasm \
       $(OUT).json \
       $(OUT).c \
       $(OUT).wrapper.js

.PHONY: all gen clean serve run check

all: gen

gen: $(GEN)

$(BUILD):
	mkdir -p $(BUILD)

$(GEN): $(FUT) | $(BUILD)
	futhark webgpu --library -o $(OUT) $(FUT)

check:
	futhark check $(FUT)

clean:
	rm -rf $(BUILD)

serve: gen
	python3 -m http.server 8000

run: serve