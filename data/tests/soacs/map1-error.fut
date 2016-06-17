-- ==
-- error:
fun *[]int main(*[][]int a) =
  -- Should be an error, because all of 'a' is consumed at the point
  -- the map is invoked.
  map(fn int (*[]int r) =>
        a[0,0],
      a)
