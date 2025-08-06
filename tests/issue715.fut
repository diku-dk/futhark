-- ==
-- input { true } output { [42,0] [0,0] }
-- input { false } output { [42,1] [1,1] }

def main (b: bool) : ([]i32, []i32) =
  let (xs, ys) =
    if b
    then (replicate 2 0, replicate 2 0)
    else (replicate 2 1, replicate 2 1)
  let xs[0] = 42
  in (xs, ys)
