-- ==
-- input {
-- }
-- output {
--   8
--   11
-- }
def f (x: (i32, i32), y: i32, z: i32) : (i32, i32) = x

def main : (i32, i32) =
  let x = 1 + 2
  let x = (x + 5, 4 + 7)
  let (x, (y, z)) = (x, x)
  in f (x, y, z)
