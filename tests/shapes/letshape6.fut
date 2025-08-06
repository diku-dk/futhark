-- A size goes out of scope and the defunctionaliser doesn't mess it
-- up.  Inspired by issue #848.
-- ==
-- input { [1,2,3] } output { [2,3] }

def main [n] (xs: [n]i32) =
  let res =
    let m = n - 1
    in map (+ 1) (take m xs)
  in res
