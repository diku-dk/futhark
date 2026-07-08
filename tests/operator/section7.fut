-- sections support mixed index+field access paths.
-- ==
-- input { [10,20] [1,2] } output { 10 2 }

def main (xs: []i32) (ys: []i32) =
  let rs = map2 (\x y -> {f = x, g = y}) xs ys
  in ( (.[0].f) rs
     , (.[1].g) rs
     )
