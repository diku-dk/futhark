-- ==
-- input { [1i64,2i64,3i64] }
-- output { [[1i64, 0i64], [4i64, 1i64], [9i64, 3i64]]}

def main (xs : []i64) =
  map (\x -> let ys = iota x
             in reduce (\y1 y2 -> [y1[0] + y2[0], y1[1] + y2[1]]) [0,0] (map (\y -> [x, y]) ys)) xs