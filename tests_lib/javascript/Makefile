FUTHARK_BACKEND ?= wasm

.PHONY: test clean

test: do_test_a do_test_array

do_test_%: test_%.js %.mjs
	node --experimental-wasm-simd test_$*.js

%.mjs: %.fut
	futhark $(FUTHARK_BACKEND) --library $^

clean:
	rm -rf *.c *.h *.class.js *.wasm *.mjs
