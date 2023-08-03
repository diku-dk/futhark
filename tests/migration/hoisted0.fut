-- GPUBody kernels are hoisted out of loops they are invariant of.
-- ==
-- structure gpu {
--   GPUBody 2
--   /GPUBody 1
--   /Loop/Loop/GPUBody/BinOp 1
-- }

def main [n] (A: *[n]i64) (x: i64) : *[n]i64 =
  loop A for i < n do
    -- Storage of x should be hoisted.
    let sum = loop x for y in #[unsafe] A[i:] do x + y
     in A with [i] = sum
