-- Test that a simple consuming map produces an error.
-- ==
-- error:

def main (a: *[][]f64) : [][]f64 =
  map (\(r: *[]f64) : *[]f64 ->
         r)
      a
