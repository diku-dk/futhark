-- Migrated statements are moved into GPUBody statements that are combined.
--
-- The consumption of dependencies are considered when statements are reordered.
-- ==
-- structure gpu {
--   /GPUBody 2
--   /GPUBody/If/True/Update 1
-- }

def one = opaque 1i64
def two = one + 1

def main (A: *[5]i64) (x: i64) : *[1]i64 =
  let A1 = A[0:3]
  -- alias #1 of A
  let A2 = A[2:5]
  -- alias #2 of A
  let y = A1[0] + A1[1]
  -- gpu 1, observes A through A1
  let z =
    if x == 0
    then -- observes A through A2
         reduce (+) 0 A2
    else y
  let C =
    if A1[0] == 0
    then -- gpu 2, consumes A through A1
         let A1' = A1 with [1] = z
         in #[unsafe] A1'[one:two]
    else #[unsafe] A1[one:two]
  in #[unsafe] C :> *[1]i64
