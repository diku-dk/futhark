-- Basic record update.
-- ==
-- input { 0 0 } output { 2 0 }
def main (x: i32) (y: i32): (i32, i32) =
  let r0 = {x, y}
  let r1 = 2 r0 with x =
  in (r1.x, r1.y)