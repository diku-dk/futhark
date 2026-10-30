-- ==
-- input {
--   1
--   2.0f32
--   3
--   4
--   5.0f32
--   6
-- }
-- output {
--   5
-- }
def tupfun (x: (i32, (f32, i32)), y: (i32, (f32, i32))) : i32 =
  let (x1, x2) = x
  let (y1, y2) = y
  in x1 + y1

--let (x0, (x1,x2)) = x in
--let (y0, (y1,y2)) = y in
--33

def main (x1: i32) (y1: f32) (z1: i32) (x2: i32) (y2: f32) (z2: i32) : i32 =
  tupfun ((x1, (y1, z1)), (x2, (y2, z2)))
