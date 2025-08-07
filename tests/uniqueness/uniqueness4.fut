-- This test inspired by code often created by
-- arrays-of-tuples-to-tuple-of-arrays transformation.
-- ==
-- input {
-- }
-- output {
--   [0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000]
--   [0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000]
-- }

def main : ([]f64, []f64) =
  let n = 10
  in loop (a, b) =
            ( replicate n 0.0
            , replicate n 0.0
            )
     for i < n do
       let a[i] = 0.0
       let b[i] = 0.0
       in (a, b)
