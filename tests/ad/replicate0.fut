-- ==
-- tags { autodiff no_webgpu }

-- ==
-- entry: fwd_vec fwd_map
-- input { 2i64 [1.0f32, 2.0f32] }
-- output { [[[1.0f32, 0.0f32], [1.0f32, 0.0f32]], [[0.0f32, 1.0f32], [0.0f32, 1.0f32]]] }

def f (n: i64) (xs: []f32) = replicate n xs

entry fwd_vec n (xs: []f32) =
  let seeds =
    map (\i -> map (\j -> f32.bool (i == j)) (indices xs)) (indices xs)
  in (jmp2 (f n) xs seeds).1

entry fwd_map n (xs: []f32) =
  map (\i -> jvp (f n) xs (map (\j -> f32.bool (i == j)) (indices xs)))
      (indices xs)

-- ==
-- entry: rev
-- input { 3i64 2i64 }
-- output { 3i64 }

entry rev n x =
  vjp (replicate n) x (iota n)
