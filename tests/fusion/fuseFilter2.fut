-- ==
-- input { [1,2,3,4] [5,6,7,8] }
-- output { 26 }
def main (a: []i32) (b: []i32) : i32 =
  let (a2, b2) = unzip (filter (\(x, y) -> x < y) (zip a b))
  in reduce (+) 0 b2
