FUTHARK = ~/.local/bin/futhark
SIZES = 1024
# 33 1024 1025 2048
all: compile compile-simple-opencl dump-simple test
allcuda: compile compile-simple-cuda dump-cuda test-cuda

compile:
	stack install --fast

compile-simple-opencl:
	$(FUTHARK) opencl tests/scan/simple.fut

compile-simple-cuda:
	$(FUTHARK) cuda tests/scan/simple.fut

run-simple:
	echo "[1,2,3,4,5,6,7,8]" | ./tests/scan/simple

dump-simple:
	./tests/scan/simple --dump-opencl tests/scan/simple-kernel.c

dump-cuda:
	./tests/scan/simple --dump-cuda tests/scan/simple-cuda-kernel.c

test: $(SIZES:%=kA-%.data)
	$(FUTHARK) test --backend=opencl tests/scan/simple.fut

test-cuda: $(SIZES:%=kA-%.data)
	$(FUTHARK) test --backend=cuda tests/scan/simple.fut

kA-%.data:
	futhark dataset --i32-bounds=-10000:10000 -g [$*]i32 > tests/scan/$@
