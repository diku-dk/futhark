-- Updates may not be migrated on their own as results from GPUBody constructs
-- are copied, which would change the asymptotic cost of the operation.
--
-- If their index depends on an array read, that read cannot be prevented.
-- ==
-- structure gpu {
--   GPUBody 0
--   /Index 1
--   /BinOp 1
--   /Update 1
-- }

def main [n] (A: *[n]i64) : *[n]i64 =
  let i = A[4]
  in A with [i] = 42
