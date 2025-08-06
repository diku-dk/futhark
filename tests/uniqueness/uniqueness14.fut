-- ==
-- input {
--   42i64
-- }
-- output {
--   [0i64, 1i64, 2i64, 3i64, 4i64, 5i64, 6i64, 7i64, 8i64, 9i64]
-- }
def f (b_1: *[]i64) : *[]i64 =
  iota (10)

def main (n: i64) : []i64 =
  let a = iota (n)
  let x = if n == 0 then a else f (a)
  in x
