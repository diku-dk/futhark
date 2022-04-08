-- Updates must not be migrated in order to retain their asymptotic cost.
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
