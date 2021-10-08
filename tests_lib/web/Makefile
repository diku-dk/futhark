FUTHARK_BACKEND ?= wasm

.PHONY: test clean

test: npm_install do_test_square

do_test_%: %.test.js %.mjs
	jest $*.test.js

%.mjs: %.fut
	futhark $(FUTHARK_BACKEND) --library $^

clean:
	rm -rf *.c *.h *.class.js *.wasm *.mjs

npm_install:
	npm install

