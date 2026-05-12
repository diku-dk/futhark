-- ==
-- error:
def main (a: *[][]i32) : *[]i32 =
  -- Should be an error, because all of 'a' is consumed at the point
  -- the map is invoked.
  map (\(r: *[]i32) : i32 ->
         a[0, 0])
      a
