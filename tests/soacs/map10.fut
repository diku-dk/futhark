-- Test that a simple consuming map produces an error.
-- ==
-- error:

def main (a: *[][]f32) : [][]f32 =
  map (\(r: *[]f32) : *[]f32 ->
         r)
      a
