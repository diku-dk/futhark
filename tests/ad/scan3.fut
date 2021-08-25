-- Scan with 2x2 matrix multiplication.
-- ==
-- entry: fwd_J rev_J
-- compiled input { [[1f32,2f32,3f32,4f32], [4f32,3f32,2f32,1f32], [1f32,2f32,3f32,4f32], [4f32,3f32,2f32,1f32]] }
-- output {
--   [[[1f32, 1f32, 1f32, 1f32],
--     [0f32, 0f32, 0f32, 0f32],
--     [0f32, 0f32, 0f32, 0f32],
--     [0f32, 0f32, 0f32, 0f32]],
--    [[6f32, 4f32, 6f32, 4f32],
--     [3f32, 3f32, 7f32, 7f32],
--     [0f32, 0f32, 0f32, 0f32],
--     [0f32, 0f32, 0f32, 0f32]],
--    [[18f32, 28f32, 18f32, 28f32],
--     [12f32, 18f32, 28f32, 42f32],
--     [13f32, 13f32, 33f32, 33f32],
--     [0f32, 0f32, 0f32, 0f32]],
--    [[128f32, 82f32, 128f32, 82f32],
--     [84f32, 54f32, 196f32, 126f32],
--     [78f32, 52f32, 198f32, 132f32],
--     [59f32, 59f32, 151f32, 151f32]]]
-- }

let mm2by2  (a1: f32, b1: f32, c1: f32, d1: f32)
            (a2: f32, b2: f32, c2: f32, d2: f32) =
  ( a1*a2 + b1*c2
  , a1*b2 + b1*d2
  , c1*a2 + d1*c2
  , c1*b2 + d1*d2
  )

let primal [n] (xs: [n](f32,f32,f32,f32)) =
  scan mm2by2 (1, 0, 0, 1) xs

let fromarrs = map (\x -> (x[0],x[1],x[2],x[3]))
let toarrs = map (\(a,b,c,d) -> [a,b,c,d])

entry fwd_J [n] (input: [n][4]f32) =
  let input = fromarrs input
  in tabulate n (\i -> jvp primal input (map (const (0,0,0,0)) input with [i] = (1,1,1,1)))
     |> map toarrs |> transpose

entry rev_J [n] (input: [n][4]f32) =
  let input = fromarrs input
  in tabulate n (\i -> vjp primal input (map (const (0,0,0,0)) input with [i] = (1,1,1,1)))
     |> map toarrs
