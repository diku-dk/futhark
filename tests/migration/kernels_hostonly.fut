-- Parallel kernels cannot be migrated into sequential kernels and thus blocks
-- the migration of parent statements.
-- ==
-- structure gpu {
--   /Loop/Loop/If 1
-- }

def main (A: [10][5]i64): i64 =
  loop x = 1 for i < A[0, 0] do
    loop x for B in A do
      if B[0] != 42
         then let sum = reduce (+) 0 B
               in sum%x + 1
         else 42