-- Replicated arrays can be migrated if they replicate a value once.
--
-- The replicates are combined into one and migrated with their outermost
-- dimension dropped, which is reintroduced by the GPUBody construct.
-- ==
-- structure gpu {
--   /GPUBody/Replicate 1
--   /Replicate 0
--   /Index 0
-- }

def main (A: [1]i32) : *[1][1]i32 =
  replicate 1 (replicate 1 A[0])
