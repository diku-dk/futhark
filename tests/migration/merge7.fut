-- Migrated statements are moved into GPUBody statements that are combined.
--
-- The consumption of dependencies are considered when statements are merged.
-- ==
-- structure gpu {
--   GPUBody/ArrayLit 1
--   GPUBody/If/True/Update 1
-- }

def one = opaque 1i64
def two = one + 1

def main (A: *[5]i32) (x: i32) : *[5]i32 =
  let B = [0, x, 2]
  let y = #[unsafe] B[one]
  let C =
    if A[0] == y
    then let B' = B with [1] = 3
         in #[unsafe] B'[one:two]
    else #[unsafe] B[one:two]
  in A with [1:2] = C
