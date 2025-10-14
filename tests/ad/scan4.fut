-- Scan with tuple operator.
-- ZeroQuadrant case
-- ==
-- tags { autodiff }
-- entry: fwd_J rev_J
-- input { [[1.0f32, 2.0f32, 3.0f32], [4.0f32, 3.0f32, 5.0f32], [3.0f32, 4.0f32, 2.0f32], [4.0f32, 2.0f32, 1.0f32]] }
-- output {
-- [[[1f32, 1f32, 1f32], [0f32, 0f32, 0f32],
--   [0f32, 0f32, 0f32], [0f32, 0f32, 0f32]],
--  [[1f32, 3f32, 0f32], [1f32, 2f32, 1f32],
--   [0f32, 0f32, 0f32], [0f32, 0f32, 0f32]],
--  [[1f32, 12f32, 0f32], [1f32, 8f32, 1f32],
--   [1f32, 6f32, 0f32], [0f32, 0f32, 0f32]],
--  [[1f32, 24f32, 0f32], [1f32, 16f32, 1f32],
--   [1f32, 12f32, 0f32], [1f32, 24f32, 0f32]]]
-- }

def primal [n] (xs: [n](f32, f32, f32)) =
  scan (\(a1, b1, c1) (a2, b2, c2) -> (a1 + a2, b1 * b2, f32.max c1 c2)) (0, 1, f32.lowest) xs

def fromarrs = map (\x -> (x[0], x[1], x[2]))
def toarrs = map (\(a, b, c) -> [a, b, c])

entry fwd_J [n] (input: [n][3]f32) =
  let input = fromarrs input
  in tabulate n (\i -> jvp primal input (replicate n (0, 0, 0) with [i] = (1, 1, 1)))
     |> map toarrs
     |> transpose

entry rev_J [n] (input: [n][3]f32) =
  let input = fromarrs input
  in tabulate n (\i -> vjp primal input (replicate n (0, 0, 0) with [i] = (1, 1, 1)))
     |> map toarrs
