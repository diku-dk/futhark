-- If statements may be migrated as a whole but only if reads are reduced.
--
-- Migrating the whole if statement in this case saves no reads as it would
-- introduce a read of the second tuple value.
-- ==
-- structure gpu {
--   GPUBody 0
-- }

def main (A: [5]i64) : (i64, i64) =
  if A[0] == 0 then (7, 42) else (A[2], 1337)
