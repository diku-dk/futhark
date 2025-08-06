-- ==
-- input {
--   10i64
-- }
-- output {
--   -1i64
-- }
def neg (x: i64) : i64 = -x

def main (a: i64) : i64 =
  let b = a + 100
  let x = iota (a)
  let c = b + 200
  let z = 3 * 2 - 6
  --let y = map(op ~, x) in
  let y = map neg x
  let d = c + 300
  in if (false)
     then d + y[1]
     else y[1]
