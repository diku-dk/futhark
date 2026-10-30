-- This test inspired by code often created by
-- arrays-of-tuples-to-tuple-of-arrays transformation.
-- ==
-- input {
-- }
-- output {
--   [0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32]
--   [0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32]
-- }

def main : ([]f32, []f32) =
  let n = 10
  in loop (a, b) =
            ( replicate n 0.0
            , replicate n 0.0
            )
     for i < n do
       let a[i] = 0.0
       let b[i] = 0.0
       in (a, b)
