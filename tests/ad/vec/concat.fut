-- ==
-- entry: fwd_vec fwd_map
-- input { [1.0, 2.0, 3.0] }
-- output { [[1.0, 0.0, 0.0, 1.0, 0.0, 0.0], [0.0, 1.0, 0.0, 0.0, 1.0, 0.0], [0.0, 0.0, 1.0, 0.0, 0.0, 1.0]] }

def f (xs: []f64) = xs ++ xs

entry fwd_vec (xs: []f64) =
  let seeds =
    map (\i -> map (\j -> f64.bool (i == j)) (indices xs)) (indices xs)
  in (jvp2_vec f xs seeds).1

entry fwd_map (xs: []f64) =
  map (\i -> jvp f xs (map (\j -> f64.bool (i == j)) (indices xs)))
      (indices xs)
