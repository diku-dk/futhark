-- ==
-- entry: satadd
-- compiled random input { [1000]u64 [100]i32 [1000]i32 } output { true true }

-- ==
-- entry: argmax
-- compiled random input { [500]u64 [50]i32 [50]i32 [500]i32 [500]i32 } output { true true true true true true true true }
def sat_add_u24 (x: i32) (y: i32) : i32 =
  let sat_val = (1 << 24) - 1
  in if sat_val - x < y
     then sat_val
     else x + y

def satadd_p [n] [m] (is: [n]i64) (dst: [m]i32, vs: [n]i32) : [m]i32 =
  reduce_by_index (copy dst) sat_add_u24 0 is vs

def satadd_fwd [n] [m] (is: [n]i64) (dst: [m]i32, vs: [n]i32) : ([m][m]i32, [m][n]i32) =
  let t1 = tabulate m (\i -> jvp (satadd_p is) (dst, vs) (replicate m 0 with [i] = 1, replicate n 0))
  let t2 = tabulate n (\i -> jvp (satadd_p is) (dst, vs) (replicate m 0, replicate n 0 with [i] = 1))
  in (transpose t1, transpose t2)

def satadd_rev [n] [m] (is: [n]i64) (dst: [m]i32, vs: [n]i32) : ([m][m]i32, [m][n]i32) =
  tabulate m (\i -> vjp (satadd_p is) (dst, vs) (replicate m 0 with [i] = 1))
  |> unzip

entry satadd [n] [m] (is': [n]u64) (dst': [m]i32) (vs': [n]i32) =
  let is = map (\i -> i64.u64 (i %% u64.i64 m)) is'
  let dst = map (%% 1000) dst'
  let vs = map (%% 1000) vs'
  let (fwd1, fwd2) = satadd_fwd is (dst, vs)
  let (rev1, rev2) = satadd_rev is (dst, vs)
  let t1 = map2 (map2 (==)) fwd1 rev1 |> map and |> and
  let t2 = map2 (map2 (==)) fwd2 rev2 |> map and |> and
  in (t1, t2)

def argmax_f (x: i32, i: i32) (y: i32, j: i32) : (i32, i32) =
  if x < y
  then (x, i)
  else if y < x
  then (y, j)
  else if i < j
  then (x, i)
  else (y, j)

def argmax_p [n] [m] (is: [n]i64) (dst_a: [m]i32, dst_b: [m]i32, vs_a: [n]i32, vs_b: [n]i32) =
  reduce_by_index (zip (copy dst_a) (copy dst_b)) argmax_f (i32.highest, i32.highest) is (zip vs_a vs_b)

def argmax_fwd [n] [m] (is: [n]i64) (dst_a: [m]i32) (dst_b: [m]i32) (vs_a: [n]i32) (vs_b: [n]i32) : ([m][m]i32, [m][m]i32, [m][n]i32, [m][n]i32, [m][m]i32, [m][m]i32, [m][n]i32, [m][n]i32) =
  let (t1, t2) =
    tabulate m (\i ->
                  jvp (argmax_p is)
                      (dst_a, dst_b, vs_a, vs_b)
                      (replicate m 0 with [i] = 1, replicate m 0, replicate n 0, replicate n 0))
    |> map unzip
    |> unzip
  let (t3, t4) =
    tabulate m (\i ->
                  jvp (argmax_p is)
                      (dst_a, dst_b, vs_a, vs_b)
                      (replicate m 0, replicate m 0 with [i] = 1, replicate n 0, replicate n 0))
    |> map unzip
    |> unzip
  let (t5, t6) =
    tabulate n (\i ->
                  jvp (argmax_p is)
                      (dst_a, dst_b, vs_a, vs_b)
                      (replicate m 0, replicate m 0, replicate n 0 with [i] = 1, replicate n 0))
    |> map unzip
    |> unzip
  let (t7, t8) =
    tabulate n (\i ->
                  jvp (argmax_p is)
                      (dst_a, dst_b, vs_a, vs_b)
                      (replicate m 0, replicate m 0, replicate n 0, replicate n 0 with [i] = 1))
    |> map unzip
    |> unzip
  in (transpose t1, transpose t2, transpose t5, transpose t6, transpose t3, transpose t4, transpose t7, transpose t8)

def argmax_rev [n] [m] (is: [n]i64) (dst_a: [m]i32) (dst_b: [m]i32) (vs_a: [n]i32) (vs_b: [n]i32) : ([m][m]i32, [m][m]i32, [m][n]i32, [m][n]i32, [m][m]i32, [m][m]i32, [m][n]i32, [m][n]i32) =
  let (t1, t2, t3, t4) =
    tabulate m (\i ->
                  vjp (argmax_p is)
                      (dst_a, dst_b, vs_a, vs_b)
                      (zip (replicate m 0 with [i] = 1) (replicate m 0)))
    |> unzip4
  let (t5, t6, t7, t8) =
    tabulate m (\i ->
                  vjp (argmax_p is)
                      (dst_a, dst_b, vs_a, vs_b)
                      (zip (replicate m 0) (replicate m 0 with [i] = 1)))
    |> unzip4
  in (t1, t2, t3, t4, t5, t6, t7, t8)

entry argmax [n] [m] (is': [n]u64) (dst_a: [m]i32) (dst_b: [m]i32) (vs_a: [n]i32) (vs_b: [n]i32) =
  let is = map (\i -> i64.u64 (i %% u64.i64 m)) is'
  let (f1, f2, f3, f4, f5, f6, f7, f8) = #[noinline] argmax_fwd is dst_a dst_b vs_a vs_b
  let (r1, r2, r3, r4, r5, r6, r7, r8) = #[noinline] argmax_rev is dst_a dst_b vs_a vs_b
  let t1 = map2 (map2 (==)) f1 r1 |> map and |> and
  let t2 = map2 (map2 (==)) f2 r2 |> map and |> and
  let t3 = map2 (map2 (==)) f3 r3 |> map and |> and
  let t4 = map2 (map2 (==)) f4 r4 |> map and |> and
  let t5 = map2 (map2 (==)) f5 r5 |> map and |> and
  let t6 = map2 (map2 (==)) f6 r6 |> map and |> and
  let t7 = map2 (map2 (==)) f7 r7 |> map and |> and
  let t8 = map2 (map2 (==)) f8 r8 |> map and |> and
  in (t1, t2, t3, t4, t5, t6, t7, t8)
