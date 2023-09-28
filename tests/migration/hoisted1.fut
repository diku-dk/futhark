-- GPUBody kernels are hoisted out of loops they are invariant of.
--
-- If merging occurs before hoisting then this test will fail.
-- ==
-- structure gpu {
--   GPUBody 1
--   /Loop/GPUBody/Index 2
--   /Loop/GPUBody/Loop/BinOp 2
-- }

def main [n] (A: *[n]i64) (x: i64) : *[n]i64 =
  loop A for i < n-1 do
    let j = #[unsafe] (A[i] + A[i+1]%n) % n
    -- Storage of x should be hoisted.
    let sum = loop x for y in #[unsafe] A[j:] do x + y
     in #[unsafe] A with [i] = sum
