-- Reduce with 2x2 matrix multiplication.
-- ==
-- tags { autodiff }
-- entry: fwd_map rev_map fwd_vec rev_vec
-- input { [[1f32,2f32,3f32,4f32], [4f32,3f32,2f32,1f32], [1f32,2f32,3f32,4f32], [4f32,3f32,2f32,1f32]] }
-- output {
--  [[[92.0f32, 36.0, 0.0, 0.0],
--   [8.0f32, 20.0, 16.0, 40.0],
--   [32.0f32, 16.0, 20.0, 10.0],
--   [23.0f32, 0.0, 36.0, 0.0]],
--  [[59.0f32, 23.0, 0.0, 0.0],
--   [5.0f32, 13.0, 10.0, 26.0],
--   [24.0f32, 8.0, 15.0, 5.0],
--   [0.0f32, 23.0, 0.0, 36.0]],
--  [[0.0f32, 0.0, 92.0, 36.0],
--   [24.0f32, 60.0, 32.0, 80.0],
--   [80.0f32, 40.0, 52.0, 26.0],
--   [59.0f32, 0.0, 92.0, 0.0]],
--  [[0.0f32, 0.0, 59.0, 23.0],
--   [15.0f32, 39.0, 20.0, 52.0],
--   [60.0f32, 20.0, 39.0, 13.0],
--   [0.0f32, 59.0, 0.0, 92.0]]]
-- }

def mm2by2 (a1: f32, b1: f32, c1: f32, d1: f32)
           (a2: f32, b2: f32, c2: f32, d2: f32) =
  ( a1 * a2 + b1 * c2
  , a1 * b2 + b1 * d2
  , c1 * a2 + d1 * c2
  , c1 * b2 + d1 * d2
  )

def primal [n] (xs: [n](f32, f32, f32, f32)) =
  reduce mm2by2 (1, 0, 0, 1) xs

def fromarr = \(x: [4]f32) -> (x[0], x[1], x[2], x[3])

def fromarrs = map fromarr
def toarrs = map (\(a, b, c, d) -> [a, b, c, d])

def onehot_1d n x =
  tabulate n (\i -> f32.bool (i == x))

def onehot_2d n m x y =
  tabulate_2d n m (\i j -> f32.bool ((i, j) == (x, y)))

entry fwd_map [n] (input: [n][4]f32) : [4][n][4]f32 =
  let input = fromarrs input
  in tabulate (n * 4) (\i -> jvp primal input (fromarrs (onehot_2d n 4 (i / 4) (i % 4))))
     |> toarrs
     |> transpose
     |> map unflatten

entry fwd_vec [n] (input: [n][4]f32) : [4][n][4]f32 =
  let input = fromarrs input
  let seeds = tabulate (n * 4) (\i -> (fromarrs (onehot_2d n 4 (i / 4) (i % 4))))
  in jvp_vec primal input seeds
     |> toarrs
     |> transpose
     |> map unflatten

entry rev_map [n] (input: [n][4]f32) : [4][n][4]f32 =
  let input = fromarrs input
  in tabulate 4 (\i -> vjp primal input (fromarr (onehot_1d 4 i)))
     |> map toarrs

entry rev_vec [n] (input: [n][4]f32) : [4][n][4]f32 =
  let input = fromarrs input
  let seeds = tabulate 4 (\i -> fromarr (onehot_1d 4 i))
  in vjp_vec primal input seeds
     |> map toarrs
