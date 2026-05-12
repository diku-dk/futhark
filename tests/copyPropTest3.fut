-- ==
-- input {
-- }
-- output {
--   70i64
-- }
def getInt () : i64 = 10

def myfun (x: (i64, i64, (i64, i64))) : i64 =
  let (a, b, (c, d)) = x in a + b + c + d

def main : i64 =
  let n = getInt ()
  let a = (n, n, (n * 0 + 5, n))
  let (x1, x2) =
    ( replicate n a
    , [a, a, a, a, a, a, a, a, a, a]
    )
  let y1 = replicate n x1
  let y2 = replicate n x2
  let z = y1[n - 1]
  let (b, c, (d, e)) = z[n / 2]
  let m = y2[0, (5 - 1) / 2]
  let p = m
  in b + c + d + e + myfun (p)
