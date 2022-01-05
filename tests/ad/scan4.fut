-- Scan with tuple operator.
-- ==
-- entry: fwd_J rev_J
-- compiled input { [[1.0f32, 2.0f32], [4.0f32, 3.0f32], [3.0f32, 4.0f32], [4.0f32, 2.0f32]] }
-- output {
-- [[[1f32, 1f32],  [0f32, 0f32],  [0f32, 0f32],  [0f32, 0f32]],
--  [[1f32, 3f32],  [1f32, 2f32],  [0f32, 0f32],  [0f32, 0f32]],
--  [[1f32, 12f32], [1f32, 8f32],  [1f32, 6f32],  [0f32, 0f32]],
--  [[1f32, 24f32], [1f32, 16f32], [1f32, 12f32], [1f32, 24f32]]]
-- }

def primal [n] (xs: [n](f32,f32)) =
  scan (\(a1,b1) (a2,b2) -> (a1+a2, b1*b2)) (0,1) xs

def fromarrs = map (\x -> (x[0],x[1]))
def toarrs = map (\(a,b) -> [a,b])

entry fwd_J [n] (input: [n][2]f32) =
  let input = fromarrs input
  in tabulate n (\i -> jvp primal input (replicate n (0,0) with [i] = (1,1)))
     |> map toarrs |> transpose

entry rev_J [n] (input: [n][2]f32) =
  let input = fromarrs input
  in tabulate n (\i -> vjp primal input (replicate n (0,0) with [i] = (1,1)))
     |> map toarrs
