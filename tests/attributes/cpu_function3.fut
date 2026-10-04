-- Test that what we get back from a cpu_function can be used on the GPU.
-- ==
-- input { 3i64 }
-- output { [1i64,2i64,3i64] }
-- structure gpu-mem { /Apply 1 /SegMap 1 }

#[noinline] #[cpu_function]
def frob (n: i64) =
  iota n

entry main n = map (+ 1) (frob n)
