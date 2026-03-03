-- sections support index+field+slice access paths.
-- ==
-- input { [[10,20,30,40],[5,6,7,8]] } output { [20,30] }

def main (xss: [][]i32) =
  let rs = map (\xs -> {a = xs}) xss
  in (.[0].a.[1:3]) rs
