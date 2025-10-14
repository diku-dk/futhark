-- ==
-- tags { autodiff }
--  entry: rev
--  compiled input {
--    [0i64,1i64,2i64,3i64,2i64,1i64,0i64,1i64,2i64]
--    [0f32,1f32,2f32,3f32,4f32]
--    [-1i64,-1i64,-1i64,-1i64,-1i64]
--    [-5f32,0f32,2f32,5f32,4f32,1f32,-2f32,-8f32,4f32]
--    [0i64,1i64,2i64,3i64,4i64,5i64,6i64,7i64,8i64] }
--  output {
--    [1f32,1f32,0f32,0f32,1f32]
--    [1i64,1i64,0i64,0i64,1i64]
--    [0f32,0f32,0f32,1f32,1f32,0f32,0f32,0f32,0f32]
--    [0i64,0i64,0i64,1i64,1i64,0i64,0i64,0i64,0i64] }

def argmax (x: f32, i: i64) (y: f32, j: i64) =
  if x == y
  then (x, i64.min i j)
  else if x > y
  then (x, i)
  else (y, j)

def f [n] [m] (is: [n]i64) (dst: [m](f32, i64), vs: [n](f32, i64)) =
  reduce_by_index (copy dst) argmax (f32.lowest, i64.highest) is vs

entry rev [n] [m] (is: [n]i64) (dst0: [m]f32) (dst1: [m]i64) (vs0: [n]f32) (vs1: [n]i64) =
  let (r1, r2) = vjp (f is) (zip dst0 dst1, zip vs0 vs1) (zip (replicate m 1) (replicate m 1))
  let (v1, i1) = unzip r1
  let (v2, i2) = unzip r2
  in (v1, i1, v2, i2)

def fvec [n] [m] [k] (is: [n]i64) (dst: [k][m](f32, i64), vs: [n][m](f32, i64)) =
  reduce_by_index (copy dst) (map2 argmax) (replicate m (f32.lowest, i64.highest)) is vs

-- ==
--  entry: revvec
--  compiled input {
--    [0i64,1i64,2i64,1i64,0i64,1i64]
--    [[1f32,2f32,3f32],[4f32,5f32,6f32],[7f32,8f32,9f32],[10f32,11f32,12f32]]
--    [[-1i64,-1i64,-1i64],[-1i64,-1i64,-1i64],[-1i64,-1i64,-1i64],[-1i64,-1i64,-1i64]]
--    [[4f32,2f32,2f32],[5f32,6f32,8f32],[9f32,8f32,6f32],[4f32,6f32,7f32],[4f32,0f32,3f32],[3f32,6f32,7f32]]
--    [[1i64,2i64,3i64],[4i64,5i64,6i64],[7i64,8i64,9i64],[10i64,11i64,12i64],[13i64,14i64,15i64],[16i64,17i64,18i64]] }
--  output {
--    [[0f32,1f32,1f32],[0f32,0f32,0f32],[0f32,1f32,1f32],[1f32,1f32,1f32]]
--    [[0i64,1i64,1i64],[0i64,0i64,0i64],[0i64,1i64,1i64],[1i64,1i64,1i64]]
--    [[1f32,0f32,0f32],[1f32,1f32,1f32],[1f32,0f32,0f32],[0f32,0f32,0f32],[0f32,0f32,0f32],[0f32,0f32,0f32]]
--    [[1i64,0i64,0i64],[1i64,1i64,1i64],[1i64,0i64,0i64],[0i64,0i64,0i64],[0i64,0i64,0i64],[0i64,0i64,0i64]] }
entry revvec [n] [m] [k] (is: [n]i64) (dst0: [k][m]f32) (dst1: [k][m]i64) (vs0: [n][m]f32) (vs1: [n][m]i64) =
  let (r1, r2) = vjp (fvec is) (map2 zip dst0 dst1, map2 zip vs0 vs1) (map2 zip (replicate k (replicate m 1)) (replicate k (replicate m 1)))
  let (v1, i1) = map unzip r1 |> unzip
  let (v2, i2) = map unzip r2 |> unzip
  in (v1, i1, v2, i2)
