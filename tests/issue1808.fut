-- ==
-- input { [1,2,3] }
-- output { [1,2,3,1,2,3] }

def main (xs: []i32) =
  let [m] ys: [m]i32 = xs ++ xs
  in map (\i -> ys[i]) (iota m)
