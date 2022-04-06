-- GPUBody kernels are hoisted out of loops they are invariant of.
--
-- If merging occurs before hoisting then this test will fail.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /DoLoop/GPUBody 1
--   /DoLoop/Update 1
-- }

def main [n] (A: *[n]i64) (x: i64) : *[n]i64 =
  loop A' = A for i < n do
    let j = #[unsafe] (A'[i] + A'[i+1]%n) % n
    in #[unsafe] A' with [j] = x -- storage of x should be hoisted.