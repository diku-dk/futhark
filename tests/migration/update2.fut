-- Scalar variables written by Updates are turned into asynchronous slice
-- copies.
--
-- (Writing the scalar value of a variable to an array is a synchronous
-- operation that is slower than asynchronously writing and copying it.)
-- ==
-- structure gpu {
--   /GPUBody 1
--   /Update 1
-- }

def main (A: *[5]i32) (x: i32) : *[5]i32 =
  A with [4] = x
