-- ==
-- entry: fwd_vec fwd_map
-- input { 0i32 [1f32, 2f32, 3f32] }
-- output { [1f32, 0f32, 0f32] }

def f (i: i32) (xs: []f32) = xs[i]

entry fwd_vec l (xs: []f32) : []f32 =
  let seeds =
    map (\i -> map (\j -> f32.bool (i == j)) (indices xs)) (indices xs)
  in (jvp2_vec (f l) xs seeds).1

entry fwd_map l (xs: []f32) : []f32 =
  map (\i -> jvp (f l) xs (map (\j -> f32.bool (i == j)) (indices xs)))
      (indices xs)
