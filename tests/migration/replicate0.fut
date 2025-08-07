-- Replicated arrays can be migrated if they replicate a value once.
--
-- replicate 1 n is equivalent to gpu { n }.
-- ==
-- structure gpu {
--   /GPUBody 1
--   Replicate 0
--   /Index 0
-- }

def main (A: [1]i32) : *[1]i32 =
  replicate 1 A[0]
