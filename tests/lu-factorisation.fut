-- Compute LU-factorisation of matrix.
-- ==
-- input {
--   [[4.0,3.0],[6.0,3.0]]
-- }
-- output {
--   [[1.000000, 0.000000],
--    [1.500000, 1.000000]]
--   [[4.000000, 3.000000],
--    [0.000000, -1.500000]]
-- }

def lu_inplace [n] (a: *[n][]f64) : (*[][]f64, *[][]f64) =
  let (_, l, u) =
    loop (a, l, u) =
           ( a
           , replicate n (replicate n 0.0)
           , replicate n (replicate n 0.0)
           )
    for k < n do
      let u[k, k] = a[k, k]
      let (l, u) =
        loop (l, u) for i < n - k do
          let l[i + k, k] = a[i + k, k] / u[k, k]
          let u[k, i + k] = a[k, i + k]
          in (l, u)
      let a =
        loop a for i < n - k do
          loop a for j < n - k do
            let a[i + k, j + k] = a[i + k, j + k] - l[i + k, k] * u[k, j + k]
            in a
      in (a, l, u)
  in (l, u)

def main (a: [][]f64) : ([][]f64, [][]f64) =
  lu_inplace (copy (a))
