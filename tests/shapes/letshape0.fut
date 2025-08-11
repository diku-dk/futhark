-- ==
-- input { [1,2,-3] } output { 2i64 }

def main (xs: []i32) =
  let [n] (xs': [n]i32) = filter (> 0) xs
  in n
