-- The result of a conditional that consumes in one branch can be consumed.
-- ==
-- input { true [0,0] }
-- output { [1,2] }
-- input { false [0,0] }
-- output { [0,2] }

def main (c: bool) (xs: *[]i32) : *[]i32 =
  let ys = if c then xs with [0] = 1 else xs
  in ys with [1] = 2
