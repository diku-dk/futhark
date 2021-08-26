-- Scan with linear function composition.
-- ==
-- tags { disable }
-- entry: fwd_J rev_J
-- compiled input { [[1f32, 2f32], [4f32, 3f32], [3f32, 4f32], [4f32, 2f32]] }
-- output {
-- [[[1f32, 1f32],   [0f32,  0f32],  [0f32, 0f32],   [0f32,  0f32]],
--  [[3f32, 3f32],   [2f32,  2f32],  [0f32, 0f32],   [0f32,  0f32]],
--  [[12f32, 12f32], [8f32,  8f32],  [8f32, 6f32],   [0f32,  0f32]],
--  [[24f32, 24f32], [16f32, 16f32], [16f32, 12f32], [32f32, 24f32]]]
-- }

let primal [n] (xs: [n](f32,f32)) =
  scan (\(a1,b1) (a2,b2) -> (a2 + b2*a1, b1*b2)) (0,1) xs

let fromarrs = map (\x -> (x[0],x[1]))
let toarrs = map (\(a,b) -> [a,b])

entry fwd_J [n] (input: [n][2]f32) =
  let input = fromarrs input
  in tabulate n (\i -> jvp primal input (replicate n (0,0) with [i] = (1,1)))
     |> transpose |> map toarrs

entry rev_J [n] (input: [n][2]f32) =
  let input = fromarrs input
  in tabulate n (\i -> vjp primal input (replicate n (0,0) with [i] = (1,1)))
     |> map toarrs
