-- ==
-- entry: add mul
-- compiled random input { [1000]i32 } output { true }

-- ==
-- entry: vecadd vecmul
-- compiled random input { [100][100]i32 } output { true }

def add_primal [n] (as: [n]i32) =
  scan (+) 0 as

def add_fwd [n] (as: [n]i32) =
  tabulate n (\i -> jvp add_primal as (replicate n 0 with [i] = 1))
  |> transpose

def add_rev [n] (as: [n]i32) =
  tabulate n (\i -> vjp add_primal as (replicate n 0 with [i] = 1))

entry add [n] (as: [n]i32) =
  let rev = add_rev as
  let fwd = add_fwd as
  in map2 (map2 (==)) rev fwd |> map (reduce (&&) true) |> reduce (&&) true

def vecadd_primal [n] [m] (as: [n][m]i32) =
  scan (map2 (+)) (replicate m 0) as

def vecadd_fwd [n] [m] (as: [n][m]i32) =
  tabulate n (\i -> jvp vecadd_primal as (replicate n (replicate m 0) with [i] = replicate m 1))
  |> transpose

def vecadd_rev [n] [m] (as: [n][m]i32) =
  tabulate n (\i -> vjp vecadd_primal as (replicate n (replicate m 0) with [i] = replicate m 1))

entry vecadd [n] [m] (as: [n][m]i32) =
  let rev = vecadd_rev as
  let fwd = vecadd_fwd as
  in map2 (map2 (map2 (==))) rev fwd |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true

def mul_primal [n] (as: [n]i32) =
  scan (*) 1 as

def mul_fwd [n] (as: [n]i32) =
  tabulate n (\i -> jvp mul_primal as (replicate n 0 with [i] = 1))
  |> transpose

def mul_rev [n] (as: [n]i32) =
  tabulate n (\i -> vjp mul_primal as (replicate n 0 with [i] = 1))

entry mul [n] (as': [n]i32) =
  let as = map (\a -> i32.abs a % 2) as'
  let rev = mul_rev as
  let fwd = mul_fwd as
  in map2 (map2 (==)) rev fwd |> map (reduce (&&) true) |> reduce (&&) true

def vecmul_primal [n] [m] (as: [n][m]i32) =
  scan (map2 (*)) (replicate m 1) as

def vecmul_fwd [n] [m] (as: [n][m]i32) =
  tabulate n (\i -> jvp vecmul_primal as (replicate n (replicate m 0) with [i] = replicate m 1))
  |> transpose

def vecmul_rev [n] [m] (as: [n][m]i32) =
  tabulate n (\i -> vjp vecmul_primal as (replicate n (replicate m 0) with [i] = replicate m 1))

entry vecmul [n] [m] (as: [n][m]i32) =
  let rev = vecmul_rev as
  let fwd = vecmul_fwd as
  in map2 (map2 (map2 (==))) rev fwd |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true
