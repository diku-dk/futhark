-- ==
-- input {
--   42i64
-- }
-- output {
--   820i64
-- }
def main (n: i64) : i64 =
  let a = iota (1)
  let a =
    loop a for i < n do
      let b = replicate n 0
      -- Error if hoisted outside loop.
      in loop c = b for j < i do let c[0] = c[0] + j in c
  in a[0]
