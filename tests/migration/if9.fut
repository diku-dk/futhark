-- If statements that return arrays can also be migrated if doing so does not
-- introduce any additional array copying.
--
-- In this case the 'if' cannot be migrated.
-- ==
-- structure gpu {
--   GPUBody 0
-- }

def main [n] (A: [n]i64) (m: i64) : [m]i64 =
  if A[0] == 0 then A[:m] else A[1:m + 1] :> [m]i64
