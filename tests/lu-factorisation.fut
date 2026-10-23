-- Compute LU-factorisation of matrix.
-- ==
-- input {
--   [[4.0f32,3.0f32],[6.0f32,3.0f32]]
-- }
-- output {
--   [[1.000000f32, 0.000000f32],
--    [1.500000f32, 1.000000f32]]
--   [[4.000000f32, 3.000000f32],
--    [0.000000f32, -1.500000f32]]
-- }

def lu_inplace [n] (a: *[n][]f32) : (*[][]f32, *[][]f32) =
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

def main (a: [][]f32) : ([][]f32, [][]f32) =
  lu_inplace (copy (a))
