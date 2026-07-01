-- ==
-- input { [2i64, 3i64] [1i64, 12i64] }
-- output { [3i64, 2i64] }

def main (ns: []i64) (is: []i64) =
  map2 (\n i ->
          let xs = unflatten_3d (iota (n * 2 * 3)) 
          let ys = transpose (map transpose (xs))
          let zs = flatten (flatten (opaque ys))
          in zs[i])
       ns is
