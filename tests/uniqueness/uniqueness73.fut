-- A loop with a tuple of consumed parameters.
-- ==
-- input { [0] [0] }
-- output { [2] [2] }

def main (xs: *[]i32) (ys: *[]i32) : (*[]i32, *[]i32) =
  loop (xs, ys) for i < 3 do (xs with [0] = i, ys with [0] = i)
