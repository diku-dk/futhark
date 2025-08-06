-- In general replicates are not migrated in order not to turn a parallel
-- operation into a sequential one (GPUBody kernels are single-threaded).
--
-- They can however be rewritten to allow the computation of their replicated
-- value to be migrated.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /Replicate 1
--   /Index 0
-- }

def main (A: [1]i32) (n: i64) : *[n][1]i32 =
  replicate n (replicate 1 A[0])
