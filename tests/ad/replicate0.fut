-- ==
-- tags { autodiff }

-- ==
-- entry: fwd_vec fwd_map
-- input { 2i64 [1.0, 2.0] }
-- output { [[[1.0, 0.0], [1.0, 0.0]], [[0.0, 1.0], [0.0, 1.0]]] }

def f (n: i64) (xs: []f64) = replicate n xs

entry fwd_vec n (xs: []f64) =
  let seeds =
    map (\i -> map (\j -> f64.bool (i == j)) (indices xs)) (indices xs)
  in (jvp2_vec (f n) xs seeds).1

entry fwd_map n (xs: []f64) =
  map (\i -> jvp (f n) xs (map (\j -> f64.bool (i == j)) (indices xs)))
      (indices xs)

-- ==
-- entry: rev
-- input { 3i64 2i64 }
-- output { 3i64 }

entry rev n x =
  vjp (replicate n) x (iota n)
