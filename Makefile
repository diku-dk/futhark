FUTHARK = ~/.local/bin/futhark
SIZES = 50000
#13 42 8704 32768 524288 1048576
# 65536 131072 262144 524288 1048576 16777216
# 33 1024 1025 2048 8704

# all: compile compile-scanomap_2-opencl dump-scanomap_2 test
all: compile compile-simple-opencl dump-simple test-simple
allcuda: compile compile-simple-cuda dump-cuda test-cuda

compile:
	stack install --fast

compile-scanomap_2-opencl:
	$(FUTHARK) opencl tests/scan/scanomap_2.fut

compile-simple-opencl:
	$(FUTHARK) opencl tests/scan/simple.fut

compile-simple-cuda:
	$(FUTHARK) cuda tests/scan/simple.fut

run-simple:
	echo "[1,2,3,4,5,6,7,8]" | ./tests/scan/simple

dump-scanomap_2:
	./tests/scan/scanomap_2 --dump-opencl tests/scan/scanomap_2-kernel.c

dump-simple:
	./tests/scan/simple --dump-opencl tests/scan/simple-kernel.c

gpu-simple:
	$(FUTHARK) dev --gpu tests/scan/simple.fut > tests/scan/simple.gpu

gpu-seg:
	$(FUTHARK) dev --gpu tests/scan/seg-scan.fut > tests/scan/seg.gpu

dump-fused:
	$(FUTHARK) dev --gpu tests/intragroup/scan0.fut > tests/intragroup/scan0.gpu
	# ./tests/intragroup/scan0 --dump-opencl tests/intragroup/scan0-kernel.c

dump-cuda:
	./tests/scan/simple --dump-cuda tests/scan/simple-cuda-kernel.c

load-cuda:
	./tests/scan/simple --load-cuda tests/scan/simple-cuda-kernel.c < tests/scan/kA-$(SIZES).data > tests/scan/simple-cuda.result
	cat tests/scan/kA-$(SIZES).data tests/scan/simple-cuda.result | ../futhark_singlepassscan_tests/compare

load-cuda-generate-data: $(SIZES:%=kA-%.data)
	./tests/scan/simple --load-cuda tests/scan/simple-cuda-kernel.c < tests/scan/kA-$(SIZES).data > tests/scan/simple-cuda.result
	cat tests/scan/kA-$(SIZES).data tests/scan/simple-cuda.result | ../futhark_singlepassscan_tests/compare

test:
	$(FUTHARK) test --backend=opencl tests/scan/scanomap_2.fut

test-simple:
	$(FUTHARK) test --backend=opencl tests/scan/simple.fut

test-cuda:
	$(FUTHARK) test --backend=cuda tests/scan/simple.fut

kA-%.data:
	futhark dataset --i32-bounds=-10000:10000 -g [$*]i32 > tests/scan/$@

ntest: $(SIZES:%=kA-%.data)
	$(FUTHARK) test --backend=cuda tests/scan/n-tests.fut


compile64:
	$(FUTHARK) opencl tests/scan/scan64.fut
dump-64:
	./tests/scan/scan64 --dump-opencl tests/scan/scan64.fut > tests/scan/scan64-kernel.c
load-64:
	./tests/scan/simple --load-opencl tests/scan/scan64-kernel.c < tests/scan/kA-131072.data


compilescanmap:
	$(FUTHARK) opencl tests/scan/scan32map32.fut
dump-scanmap:
	./tests/scan/scan64 --dump-opencl tests/scan/scan32map32.fut > tests/scan/scan32map32-kernel.c
load-scanmap:
	./tests/scan/simple --load-opencl tests/scan/scan32map32-kernel.c < tests/scan/kA-131072.data
