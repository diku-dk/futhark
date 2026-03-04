-- Test atomic max operations in WebGPU.
-- Tests reduce_by_index with max operator (uses atomicMax).

-- ==
-- input  { [0i32, 0i32, 0i32, 0i32, 0i32]
--          [0i64, 0i64, 0i64, 0i64, 0i64]
--          [1i32, 2i32, 3i32, 4i32, 5i32] }
-- output { [5i32, 0i32, 0i32, 0i32, 0i32] }

-- ==
-- input  { [0i32, 0i32, 0i32, 0i32, 0i32]
--          [0i64, 1i64, 2i64, 3i64, 4i64]
--          [5i32, 4i32, 3i32, 2i32, 1i32] }
-- output { [5i32, 4i32, 3i32, 2i32, 1i32] }

-- ==
-- input  { [-1000i32, -1000i32, -1000i32]
--          [0i64, 0i64, 1i64, 1i64, 2i64]
--          [-5i32, -3i32, -10i32, -2i32, -1i32] }
-- output { [-3i32, -2i32, -1i32] }

-- ==
-- input  { [100i32, 200i32, 300i32]
--          [0i64, 1i64, 2i64, 0i64, 1i64, 2i64]
--          [50i32, 250i32, 150i32, 150i32, 100i32, 350i32] }
-- output { [150i32, 250i32, 350i32] }

def main [n] [m] (hist: *[n]i32) (is: [m]i64) (vs: [m]i32) : [n]i32 =
  reduce_by_index hist i32.max i32.lowest is vs
