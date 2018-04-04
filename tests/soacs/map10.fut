-- Test that a simple consuming map produces an error.
-- ==
-- error:

let main(a: *[][]f64): [][]f64 =
  map (\(r: *[]f64): *[]f64  ->
        r) a
