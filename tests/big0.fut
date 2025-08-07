-- Testing big arrays.
-- ==
-- tags { no_python no_pyopencl }
-- no_python no_opencl no_cuda no_cudatc no_hip no_wasm no_ispc compiled input { 2i64 1100000000i64 1 1073741823 } output { -2i8 }
-- no_python no_opencl no_cuda no_cudatc no_hip no_wasm no_ispc compiled input { 3i64 1073741824i64 2 1073741823 } output { -3i8 }
-- structure gpu-mem { SegMap 1  }

def main (n: i64) (m: i64) (i: i32) (j: i32) =
  -- The opaque is just to force manifestation.
  (opaque (tabulate_2d n m (\i j -> i8.i64 (i ^ j))))[i, j]
