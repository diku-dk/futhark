-- Test that builtin operations like iota and replicate still work inside CPU
-- functions.
-- ==
-- input { 3i64 }
-- output { [2i64, 3i64, 4i64] [42, 42, 42] }
-- structure gpu-mem { /Apply 1 }

#[noinline] #[cpu_function]
def frob (n: i64) =
  (map (+ 2) (iota n), replicate n 42i32)

entry main = frob
