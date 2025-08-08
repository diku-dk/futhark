-- Duplicate scalar migrations are eliminated.
--
-- Both x's are migrated and should be reduced to a single GPUBody.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /Loop/If/True/GPUBody 0
-- }

def main (A: [5]i32) (x: i32) : i32 =
  let (_, res) =
    loop (c, _) = (0, x)
    for z in A do
      if c == 3 then (c, x) else (c + 1, z)
  in res
