-- Rotates may not be migrated on their own as results from GPUBody constructs
-- are copied, which would change the asymptotic cost of the operation.
--
-- If their rotation argument depends on an array read, that read cannot be
-- prevented.
-- ==
-- structure gpu {
--   GPUBody 0
--   Rotate 1
-- }

def main (A: [5]i64) : [5]i64 =
  rotate A[0] A
