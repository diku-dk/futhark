-- Whole loops are not migrated if the asymptotic cost would change.
--
-- In this case migrating the whole loop could cause a copy of A.
-- ==
-- structure gpu {
--   /DoLoop 1
-- }

def main (A: [5]i64) : [5]i64 =
  loop B = A for i < A[0] do
    if i%4 != 0 then B :> [5]i64 else [1, 2, i, 4, 5]

