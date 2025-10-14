-- In general iotas are not migrated in order not to turn a parallel operation
-- into a sequential one (GPUBody kernels are single-threaded).
-- ==
-- structure gpu {
--   GPUBody 0
--   Iota 1
-- }

def main (A: [5]i64) : []i64 =
  iota A[0]
