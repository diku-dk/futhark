-- ==

def main [n] b (xs: *[n]i32) =
  let vals = map (+ 2) (if b then reverse xs else xs)
  in scatter xs (iota n) vals
