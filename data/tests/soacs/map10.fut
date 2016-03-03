-- Test that a simple consuming map works.
-- ==
-- input {
--   [[1.0,2.0],[1.0,2.0]]
-- }
-- output {
--   [[1.000000, 2.000000], [1.000000, 2.000000]]
-- }
fun [[f64]] main(*[[f64]] a) =
  map(fn *[f64] (*[f64] r) =>
        r,
      a)
