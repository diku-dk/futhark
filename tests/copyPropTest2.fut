-- ==
-- input {
-- }
-- output {
--   91i64
--   126i64
-- }
-- structure { Replicate 0 }

def getInt () : i64 = 10
def plus1 (x: []i32) : []i32 = map (\(y: i32) : i32 -> y + 1) x

def main : (i64, i64) =
  let n = getInt ()
  -- Int
  let x = iota (n)
  -- [#n]Int
  let m = (n * (5 - 4))
  let y = copy (replicate n x)
  -- [#n][#n]Int copy necessary as y otherwise aliases x.
  let z = copy (replicate (n + n) y)
  -- [[#n][#n]Int,m+n]; copy necessary as z otherwise aliases x.
  let q = z[n - 2]
  -- [#n][#n]Int

  let (m, x) =
    loop ((m, x)) for i < n - 1 do
      let x[i] = (m * 1)
      let m = m + x[i + 1]
      let m = m + z[n - 1, n - 2, i]
      in (m, x)
  let qq = m * (2 - 1)
  in (qq, m + x[n / 2])
