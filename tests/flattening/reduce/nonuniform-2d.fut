-- A materialised irregular 2D array (variant inner size) used as the input to a
-- redomap. The redomap width is uniform, but its input array is not, so it must
-- not take the uniform alternative - that would lift the input to a regular
-- array with an out-of-scope variant size.
-- ==
-- input { [2i64, 3i64, 0i64, 5i64] } output { [9i64, 18i64, 0i64, 45i64] }

def main [n] (sizes: [n]i64) : [n]i64 =
  map (\sz ->
         let a = opaque (tabulate_2d 3 sz (\i j -> i + j))
         in i64.sum (map i64.sum a))
      sizes
