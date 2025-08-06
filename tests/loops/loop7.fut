-- This loop is intended to trigger a bug in the in-place-lowering
-- optimisation.  It requires proper maintaining of the loop result
-- ordering.
-- ==

def main (n: i64, i: i32, x: f64) : [][]f64 =
  let res = replicate n (replicate n 0.0)
  let (u, uu) =
    ( replicate n 0.0
    , replicate n 0.0
    )
  let (u, x) =
    loop ((u, x)) for i < n - 1 do
      let y = x + 1.0
      let u[i] = u[i] * y
      in (u, y)
  let res[i] = u
  in res
