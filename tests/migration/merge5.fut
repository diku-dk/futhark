-- Migrated statements are moved into GPUBody statements that are combined.
--
-- The consumption of dependencies are considered when statements are reordered.
-- ==
-- structure gpu {
--   GPUBody 2
-- }

def main (A: *[5]i32) (x: i32) : *[3]i32 =
  let A1 = A[0:3]
  -- alias #1 of A
  let A2 = A[2:5]
  -- alias #2 of A
  let i = x % 3
  let y = #[unsafe] A1[i] + 1
  -- gpu 1, observes A through A1
  let B = A2 with [0] = 42
  -- consumes A through A2
  let z = B[2] + y
  -- gpu 2, depends on B, cannot merge with gpu 1
  let C = B with [1] = z
  in C :> *[3]i32
