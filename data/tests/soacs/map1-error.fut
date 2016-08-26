-- ==
-- error:
fun main(a: *[][]int): *[]int =
  -- Should be an error, because all of 'a' is consumed at the point
  -- the map is invoked.
  map(fn (r: *[]int): int  =>
        a[0,0],
      a)
