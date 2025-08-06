-- Array literals are migrated if they contain free scalar variables.
--
-- [n] is equivalent to gpu { n }.
-- ==
-- structure gpu {
--   /GPUBody 1
--   ArrayLit 0
--   Replicate 0
--   /Index 0
-- }

def main (x: [1]i32) : *[1]i32 =
  [x[0]]
