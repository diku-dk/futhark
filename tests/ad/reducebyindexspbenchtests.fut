-- ==
-- tags { autodiff }

-- ==
-- entry: add mul max
-- compiled random input { [100]i32 [1000]u64 [1000]i32 }

-- ==
-- entry: vecadd vecmul vecmax
-- compiled random input { [10][100]i32 [100]u64 [100][100]i32 } output { true }
def add_p [n] [m] (is: [n]i64) (dst: [m]i32, vs: [n]i32) : [m]i32 =
  reduce_by_index (copy dst) (+) 0 is vs

def add_rev [n] [m] (dst: [m]i32) (is: [n]i64) (vs: [n]i32) =
  tabulate m (\i -> vjp (add_p is) (dst, vs) (replicate m 0 with [i] = 1))
  |> unzip

def add_fwd [n] [m] (dst: [m]i32) (is: [n]i64) (vs: [n]i32) =
  let t1 = tabulate m (\i -> jvp (add_p is) (dst, vs) (replicate m 0 with [i] = 1, replicate n 0))
  let t2 = tabulate n (\i -> jvp (add_p is) (dst, vs) (replicate m 0, replicate n 0 with [i] = 1))
  in (transpose t1, transpose t2)

entry add [n] [m] (dst: [m]i32) (is': [n]u64) (vs: [n]i32) =
  let is = map (\i -> i64.u64 (i %% u64.i64 m)) is'
  let (fwd1, fwd2) = add_fwd dst is vs
  let (rev1, rev2) = add_rev dst is vs
  let t1 = map2 (map2 (==)) fwd1 rev1 |> map (reduce (&&) true) |> reduce (&&) true
  let t2 = map2 (map2 (==)) fwd2 rev2 |> map (reduce (&&) true) |> reduce (&&) true
  in t1 && t2

def mul_p [n] [m] (is: [n]i64) (dst: [m]i32, vs: [n]i32) : [m]i32 =
  reduce_by_index (copy dst) (*) 1 is vs

def mul_rev [n] [m] (dst: [m]i32) (is: [n]i64) (vs: [n]i32) =
  tabulate m (\i -> vjp (mul_p is) (dst, vs) (replicate m 0 with [i] = 1))
  |> unzip

def mul_fwd [n] [m] (dst: [m]i32) (is: [n]i64) (vs: [n]i32) =
  let t1 = tabulate m (\i -> jvp (mul_p is) (dst, vs) (replicate m 0 with [i] = 1, replicate n 0))
  let t2 = tabulate n (\i -> jvp (mul_p is) (dst, vs) (replicate m 0, replicate n 0 with [i] = 1))
  in (transpose t1, transpose t2)

entry mul [n] [m] (dst: [m]i32) (is': [n]u64) (vs: [n]i32) =
  let is = map (\i -> i64.u64 (i %% u64.i64 m)) is'
  let (fwd1, fwd2) = mul_fwd dst is vs
  let (rev1, rev2) = mul_rev dst is vs
  let t1 = map2 (map2 (==)) fwd1 rev1 |> map (reduce (&&) true) |> reduce (&&) true
  let t2 = map2 (map2 (==)) fwd2 rev2 |> map (reduce (&&) true) |> reduce (&&) true
  in t1 && t2

def max_p [n] [m] (is: [n]i64) (dst: [m]i32, vs: [n]i32) : [m]i32 =
  reduce_by_index (copy dst) i32.max i32.lowest is vs

def max_rev [n] [m] (dst: [m]i32) (is: [n]i64) (vs: [n]i32) =
  tabulate m (\i -> vjp (max_p is) (dst, vs) (replicate m 0 with [i] = 1))
  |> unzip

def max_fwd [n] [m] (dst: [m]i32) (is: [n]i64) (vs: [n]i32) =
  let t1 = tabulate m (\i -> jvp (max_p is) (dst, vs) (replicate m 0 with [i] = 1, replicate n 0))
  let t2 = tabulate n (\i -> jvp (max_p is) (dst, vs) (replicate m 0, replicate n 0 with [i] = 1))
  in (transpose t1, transpose t2)

entry max [n] [m] (dst: [m]i32) (is': [n]u64) (vs: [n]i32) =
  let is = map (\i -> i64.u64 (i %% u64.i64 m)) is'
  let (fwd1, fwd2) = max_fwd dst is vs
  let (rev1, rev2) = max_rev dst is vs
  let t1 = map2 (map2 (==)) fwd1 rev1 |> map (reduce (&&) true) |> reduce (&&) true
  let t2 = map2 (map2 (==)) fwd2 rev2 |> map (reduce (&&) true) |> reduce (&&) true
  in t1 && t2

def vecadd_p [n] [m] [k] (is: [n]i64) (dst: [m][k]i32, vs: [n][k]i32) : [m][k]i32 =
  reduce_by_index (copy dst) (map2 (+)) (replicate k 0) is vs

def vecadd_rev [n] [m] [k] (dst: [m][k]i32) (is: [n]i64) (vs: [n][k]i32) =
  tabulate m (\i -> vjp (vecadd_p is) (dst, vs) (replicate m (replicate k 0) with [i] = replicate k 1))
  |> unzip

def vecadd_fwd [n] [m] [k] (dst: [m][k]i32) (is: [n]i64) (vs: [n][k]i32) =
  let t1 = tabulate m (\i -> jvp (vecadd_p is) (dst, vs) (replicate m (replicate k 0) with [i] = replicate k 1, replicate n (replicate k 0)))
  let t2 = tabulate n (\i -> jvp (vecadd_p is) (dst, vs) (replicate m (replicate k 0), replicate n (replicate k 0) with [i] = replicate k 1))
  in (transpose t1, transpose t2)

entry vecadd [n] [m] [k] (dst: [m][k]i32) (is': [n]u64) (vs: [n][k]i32) =
  let is = map (\i -> i64.u64 (i %% u64.i64 m)) is'
  let (fwd1, fwd2) = vecadd_fwd dst is vs
  let (rev1, rev2) = vecadd_rev dst is vs
  let t1 = map2 (map2 (map2 (==))) fwd1 rev1 |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true
  let t2 = map2 (map2 (map2 (==))) fwd2 rev2 |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true
  in t1 && t2

def vecmul_p [n] [m] [k] (is: [n]i64) (dst: [m][k]i32, vs: [n][k]i32) : [m][k]i32 =
  reduce_by_index (copy dst) (map2 (*)) (replicate k 1) is vs

def vecmul_rev [n] [m] [k] (dst: [m][k]i32) (is: [n]i64) (vs: [n][k]i32) =
  tabulate m (\i -> vjp (vecmul_p is) (dst, vs) (replicate m (replicate k 0) with [i] = replicate k 1))
  |> unzip

def vecmul_fwd [n] [m] [k] (dst: [m][k]i32) (is: [n]i64) (vs: [n][k]i32) =
  let t1 = tabulate m (\i -> jvp (vecmul_p is) (dst, vs) (replicate m (replicate k 0) with [i] = replicate k 1, replicate n (replicate k 0)))
  let t2 = tabulate n (\i -> jvp (vecmul_p is) (dst, vs) (replicate m (replicate k 0), replicate n (replicate k 0) with [i] = replicate k 1))
  in (transpose t1, transpose t2)

entry vecmul [n] [m] [k] (dst': [m][k]i32) (is': [n]u64) (vs': [n][k]i32) =
  let is = map (\i -> i64.u64 (i %% u64.i64 m)) is'
  let dst = map (map (\x -> (x %% 2) + 1)) dst'
  let vs = map (map (\x -> (x %% 2) + 1)) vs'
  let (fwd1, fwd2) = vecmul_fwd dst is vs
  let (rev1, rev2) = vecmul_rev dst is vs
  let t1 = map2 (map2 (map2 (==))) fwd1 rev1 |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true
  let t2 = map2 (map2 (map2 (==))) fwd2 rev2 |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true
  in t1 && t2

def vecmax_p [n] [m] [k] (is: [n]i64) (dst: [m][k]i32, vs: [n][k]i32) : [m][k]i32 =
  reduce_by_index (copy dst) (map2 i32.max) (replicate k i32.lowest) is vs

def vecmax_rev [n] [m] [k] (dst: [m][k]i32) (is: [n]i64) (vs: [n][k]i32) =
  tabulate m (\i -> vjp (vecmax_p is) (dst, vs) (replicate m (replicate k 0) with [i] = replicate k 1))
  |> unzip

def vecmax_fwd [n] [m] [k] (dst: [m][k]i32) (is: [n]i64) (vs: [n][k]i32) =
  let t1 = tabulate m (\i -> jvp (vecmax_p is) (dst, vs) (replicate m (replicate k 0) with [i] = replicate k 1, replicate n (replicate k 0)))
  let t2 = tabulate n (\i -> jvp (vecmax_p is) (dst, vs) (replicate m (replicate k 0), replicate n (replicate k 0) with [i] = replicate k 1))
  in (transpose t1, transpose t2)

entry vecmax [n] [m] [k] (dst: [m][k]i32) (is': [n]u64) (vs: [n][k]i32) =
  let is = map (\i -> i64.u64 (i %% u64.i64 m)) is'
  let (fwd1, fwd2) = vecmax_fwd dst is vs
  let (rev1, rev2) = vecmax_rev dst is vs
  let t1 = map2 (map2 (map2 (==))) fwd1 rev1 |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true
  let t2 = map2 (map2 (map2 (==))) fwd2 rev2 |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true
  in t1 && t2
