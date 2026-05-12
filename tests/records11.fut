-- We can index the result of a projection.
-- ==
-- input { [1,2,3] 4 }
-- output { 2 }

def main (x: []i32) (y: i32) =
  let t = (x, y)
  in t.0[1]
