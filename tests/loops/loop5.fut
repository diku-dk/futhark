-- ==
-- input {
-- }
-- output {
--   [0i64, 1i64, 3i64, 6i64, 10i64, 15i64, 21i64, 28i64, 36i64, 45i64]
-- }
def main : []i64 =
  let n = 10
  let x = iota (n)
  in loop (x) for i < n - 1 do
       let x[i + 1] = x[i + 1] + x[i]
       in x
