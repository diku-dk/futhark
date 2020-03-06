FUTHARK = ~/.local/bin/futhark
SIZES = 1024
# 33 1024 1025 2048
all: compile compile-simple-opencl dump-simple test

compile:
	stack install --fast

compile-simple-opencl:
	$(FUTHARK) opencl tests/scan/simple.fut

compile-simple-cuda:
	$(FUTHARK) cuda tests/scan/simple.fut

run-simple:
	echo "[1,2,3,4,5,6,7,8]" | ./simple

dump-simple:
	./simple --dump-opencl simple-kernel.c

test: $(SIZES:%=kA-%.data)
	$(FUTHARK) test --backend=opencl tests/scan/simple.fut

kA-%.data:
	futhark dataset --i32-bounds=-10000:10000 -g [$*]i32 > $@
