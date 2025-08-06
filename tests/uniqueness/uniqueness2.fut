-- ==
-- input {
-- }
-- output {
--   3
-- }
def main : i32 =
  let n = 10
  let (a, b) = (replicate n 0, replicate n 0)
  let a[0] = 1
  let b[0] = 2
  in a[0] + b[0]
