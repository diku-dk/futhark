-- ==
-- entry: fwd_vec fwd_map
-- input { [1f32, 2f32, 3f32] }
-- output { [6f32, 3f32, 2f32] }

def f (xs: []f32) = f32.product xs

entry fwd_vec (xs: []f32) : []f32 =
  let seeds =
    map (\i -> map (\j -> f32.bool (i == j)) (indices xs)) (indices xs)
  in (jvp2_vec f xs seeds).1

entry fwd_map (xs: []f32) : []f32 =
  map (\i -> jvp f xs (map (\j -> f32.bool (i == j)) (indices xs)))
      (indices xs)
