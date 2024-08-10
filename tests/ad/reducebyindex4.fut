-- ==
--  entry: rev
--  input {
--    [ 0i64, 1i64, 2i64, 1i64, 0i64, 1i64, 2i64, 1i64, 0i64]
--    [ 1f32, 2f32, 3f32, 4f32, 5f32, 6f32, 7f32, 8f32, 9f32]
--    [10f32,11f32,12f32,13f32,14f32,15f32,16f32,17f32,18f32] }
--  output {
--    [252f32,3315f32,15f32,2805f32,180f32,2431f32,12f32,2145f32,140f32]
--    [468f32,7221f32,22f32,5757f32,288f32,4765f32,15f32,4053f32,204f32] }

def op (a1:f32,b1:f32) (a2:f32,b2:f32) : (f32,f32) =
  (b1*a2+b2*a1, b1*b2)

def f [n] (is: [n]i64) (vs: [n](f32,f32)) =
  reduce_by_index (replicate 4 (0,1)) op (0,1) is vs

entry rev [n] (is: [n]i64) (vs0: [n]f32) (vs1: [n]f32) =
  vjp (f is) (zip vs0 vs1) (replicate 4 (1,1))
  |> unzip

entry fwd [n] (is: [n]i64) (vs0: [n]f32) (vs1: [n]f32) =
  tabulate n (\i -> jvp (f is) (zip vs0 vs1) (replicate n (0,0) with [i] = (1,1)))
  |> transpose |> map unzip |> unzip