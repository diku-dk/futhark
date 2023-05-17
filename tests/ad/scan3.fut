-- Scan with 2x2 matrix multiplication.
-- MatrixMul case
-- ==
-- entry: fwd_J rev_J
-- compiled input { [[1f32,2f32,3f32,4f32], [4f32,3f32,2f32,1f32], [1f32,2f32,3f32,4f32], [4f32,3f32,2f32,1f32]] }
-- output {
-- [[[[1f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32]],
--   [[0f32, 1f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32]],
--   [[0f32, 0f32, 1f32, 0f32], [0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32]],
--   [[0f32, 0f32, 0f32, 1f32], [0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32]]],
--  [[[4f32, 2f32, 0f32, 0f32], [1f32, 0f32, 2f32, 0f32], [0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32]],
--   [[3f32, 1f32, 0f32, 0f32], [0f32, 1f32, 0f32, 2f32], [0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32]],
--   [[0f32, 0f32, 4f32, 2f32], [3f32, 0f32, 4f32, 0f32], [0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32]],
--   [[0f32, 0f32, 3f32, 1f32], [0f32, 3f32, 0f32, 4f32], [0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32]]],
--  [[[13f32, 5f32, 0f32, 0f32], [1f32, 3f32, 2f32, 6f32], [8f32, 0f32, 5f32, 0f32], [0f32, 0f32, 0f32, 0f32]],
--   [[20f32, 8f32, 0f32, 0f32], [2f32, 4f32, 4f32, 8f32], [0f32, 8f32, 0f32, 5f32], [0f32, 0f32, 0f32, 0f32]],
--   [[0f32, 0f32, 13f32, 5f32], [3f32, 9f32, 4f32, 12f32], [20f32, 0f32, 13f32, 0f32], [0f32, 0f32, 0f32, 0f32]],
--   [[0f32, 0f32, 20f32, 8f32], [6f32, 12f32, 8f32, 16f32], [0f32, 20f32, 0f32, 13f32], [0f32, 0f32, 0f32, 0f32]]],
--  [[[92f32, 36f32, 0f32, 0f32], [8f32, 20f32, 16f32, 40f32], [32f32, 16f32, 20f32, 10f32], [23f32, 0f32, 36f32, 0f32]],
--   [[59f32, 23f32, 0f32, 0f32], [5f32, 13f32, 10f32, 26f32], [24f32, 8f32, 15f32, 5f32], [0f32, 23f32, 0f32, 36f32]],
--   [[0f32, 0f32, 92f32, 36f32], [24f32, 60f32, 32f32, 80f32], [80f32, 40f32, 52f32, 26f32], [59f32, 0f32, 92f32, 0f32]],
--   [[0f32, 0f32, 59f32, 23f32], [15f32, 39f32, 20f32, 52f32], [60f32, 20f32, 39f32, 13f32], [0f32, 59f32, 0f32, 92f32]]]]
-- }

def mm2by2  (a1: f32, b1: f32, c1: f32, d1: f32)
            (a2: f32, b2: f32, c2: f32, d2: f32) =
  ( a1*a2 + b1*c2
  , a1*b2 + b1*d2
  , c1*a2 + d1*c2
  , c1*b2 + d1*d2
  )

def primal [n] (xs: [n](f32,f32,f32,f32)) =
  scan mm2by2 (1, 0, 0, 1) xs

def fromarrs = map (\(x: [4]f32) -> (x[0],x[1],x[2],x[3]))
def toarrs = map (\(a,b,c,d) -> [a,b,c,d])

def onehot_2d n m x y =
  tabulate_2d n m (\i j -> f32.bool((i,j) == (x,y)))

entry fwd_J [n] (input: [n][4]f32) : [n][4][n][4]f32 =
  let input = fromarrs input
  in tabulate (n*4) (\i -> jvp primal input (fromarrs (onehot_2d n 4 (i/4) (i%4))))
     |> map toarrs |> transpose |> map transpose |> map (map unflatten)

entry rev_J [n] (input: [n][4]f32) : [n][4][n][4]f32 =
  let input = fromarrs input
  in tabulate (n*4) (\i -> vjp primal input (fromarrs (onehot_2d n 4 (i/4) (i%4))))
     |> unflatten |> map (map toarrs)
