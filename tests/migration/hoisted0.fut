-- GPUBody kernels are hoisted out of loops they are invariant of.
-- ==
-- structure gpu {
--   GPUBody 1
--   /GPUBody 1
--   /DoLoop/Update 1
-- }

def main [n] (A: *[n]i64) (x: i64) : *[n]i64 =
  loop A' = A for i < n do
    A' with [i] = x -- storage of x should be hoisted.
