-- Nested record update.
-- ==
-- input { 0 0 } output { 1 0 0 0 }
def main (x: i32) (y: i32): (i32, i32, i32, i32) =
  let r0 = {a = {x, y}, b = {x, y}}
  let r1 = 1 r0 with a.x =
  in (r1.a.x, r1.a.y, r1.b.x, r1.b.y)