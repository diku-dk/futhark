-- ==
-- input {
-- }
-- output {
--   6
-- }
def main : i32 =
  let a = [(1, (2, 3))]
  let c = concat a a
  let (x, (y, z)) = c[1]
  in x + y + z
