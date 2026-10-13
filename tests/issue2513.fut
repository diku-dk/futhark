-- ==
-- input { 3i64 }
-- output { [1.414213562373095] }

def sa (bellman: [1]f64 -> [1]f64) (V0: [1]f64) (sa_max: i64) (bet: f64) : [1]f64 =
  let sa_min = 2i64
  let sa_tol = 1e-3
  let tol_ratio = 1e-3
  let (res, _, _, _) =
    loop (res, conv, i, tol) = (V0, false, 0i64, 0f64)
    while !conv && i < sa_max do
      let V = bellman res
      let tol' = f64.maximum (map2 (\a b -> f64.abs (a - b)) V res)
      let rtol' = if i == 1 then 1f64 else tol' / tol
      let conv =
        (i > sa_min && f64.abs (bet - rtol') < tol_ratio)
        || (i > sa_min && tol' < sa_tol)
      in (V, conv, i + 1, tol')
  in res

entry main (sa_max: i64) : [1]f64 =
  sa (\x -> [0.5 * (x[0] + 2 / x[0])]) [1.4] sa_max 0
