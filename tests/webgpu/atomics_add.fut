-- Test atomic add operations in WebGPU.
-- WebGPU has a specific atomic model that differs from other backends.
-- Tests reduce_by_index with addition operator (uses atomicAdd).

-- ==
-- input  { [0i32, 0i32, 0i32, 0i32, 0i32]
--          [0i64, 0i64, 0i64, 0i64, 0i64]
--          [1i32, 1i32, 1i32, 1i32, 1i32] }
-- output { [5i32, 0i32, 0i32, 0i32, 0i32] }

-- ==
-- input  { [0i32, 0i32, 0i32, 0i32, 0i32]
--          [0i64, 1i64, 2i64, 3i64, 4i64]
--          [1i32, 2i32, 3i32, 4i32, 5i32] }
-- output { [1i32, 2i32, 3i32, 4i32, 5i32] }

-- ==
-- input  { [0i32, 0i32, 0i32, 0i32, 0i32]
--          [0i64, 0i64, 1i64, 1i64, 2i64]
--          [1i32, 2i32, 3i32, 4i32, 5i32] }
-- output { [3i32, 7i32, 5i32, 0i32, 0i32] }

-- ==
-- input  { [10i32, 20i32, 30i32]
--          [0i64, 1i64, 2i64, 0i64, 1i64, 2i64]
--          [1i32, 2i32, 3i32, 4i32, 5i32, 6i32] }
-- output { [15i32, 27i32, 39i32] }

-- out of bounds indices should be ignored
-- ==
-- input  { [0i32, 0i32, 0i32]
--          [0i64, 1i64, 100i64, -1i64, 2i64]
--          [1i32, 2i32, 3i32, 4i32, 5i32] }
-- output { [1i32, 2i32, 5i32] }

def main [n] [m] (hist: *[n]i32) (is: [m]i64) (vs: [m]i32) : [n]i32 =
  reduce_by_index hist (+) 0i32 is vs
