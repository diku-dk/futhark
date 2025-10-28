-- Duplicate scalar migrations are eliminated.
--
-- Both x's are migrated and should be reduced to a single array.
-- They are migrated with the GPUBody that computes 'A[1]+1'.
-- If merging occurs before CSE then this test will fail.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /Loop/If/True/GPUBody 0
-- }

def main (A: *[5]i32) (x: i32) : i32 =
  let A[0] = A[1] + 1
  --
  let (_, res) =
    loop (c, _) = (0, x)
    for z in A do
      if c == 3 then (c, x) else (c + 1, z)
  in res
