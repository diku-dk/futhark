-- Test that complex shadowing does not break alias analysis.
-- ==
-- input { 10i64 }
-- output {
--   [0i64, 1i64, 2i64, 3i64, 4i64, 5i64, 6i64, 7i64, 8i64, 9i64]
-- }

def main n: []i64 =
  let a = iota(n)
  let c = let (a, b) = (iota(n), a) let a[0] = 42 in a
  in a -- OK, because the outer a was never consumed.
