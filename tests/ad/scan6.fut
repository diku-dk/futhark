-- Scan with linear function composition.
-- ==
-- entry: fwd_J rev_J
-- compiled input { [[1f32, 2f32], [4f32, 3f32], [3f32, 4f32], [4f32, 2f32]] }
-- output {
-- [[[1f32, 0f32], [3f32, 0f32], [12f32, 0f32], [24f32, 0f32]],
--  [[0f32, 1f32], [0f32, 3f32], [0f32, 12f32], [0f32, 24f32]],
--  [[0f32, 0f32], [1f32, 0f32], [4f32, 0f32], [8f32, 0f32]],
--  [[0f32, 0f32], [1f32, 2f32], [4f32, 8f32], [8f32, 16f32]],
--  [[0f32, 0f32], [0f32, 0f32], [1f32, 0f32], [2f32, 0f32]],
--  [[0f32, 0f32], [0f32, 0f32], [7f32, 6f32], [14f32, 12f32]],
--  [[0f32, 0f32], [0f32, 0f32], [0f32, 0f32], [1f32, 0f32]],
--  [[0f32, 0f32], [0f32, 0f32], [0f32, 0f32], [31f32, 24f32]]]
-- }

let primal [n] (xs: [n](f32,f32)) =
  scan (\(a1,b1) (a2,b2) -> (a2 + b2*a1, b1*b2)) (0,1) xs

let fromarrs = map (\x -> (x[0],x[1]))
let toarrs = map (\(a,b) -> [a,b])

let onehot_2d n m x y =
  tabulate_2d n m (\i j -> f32.bool((i,j) == (x,y)))

entry fwd_J [n] (input: [n][2]f32) =
  let input = fromarrs input
  in tabulate (n*2) (\i -> jvp primal input (fromarrs (onehot_2d n 2 (i/2) (i%2))))
     |> map toarrs

entry rev_J [n] (input: [n][2]f32) =
  let input = fromarrs input
  in tabulate (n*2) (\i -> jvp primal input (fromarrs (onehot_2d n 2 (i/2) (i%2))))
     |> map toarrs
