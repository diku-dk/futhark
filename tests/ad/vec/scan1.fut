-- Scan with addition.
-- ==
-- tags { autodiff }
-- entry: fwd_vec fwd_map rev_map rev_vec
-- input { [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32] }
-- output { [[1f32, 1f32, 1f32, 1f32, 1f32],
--           [0f32, 1f32, 1f32, 1f32, 1f32],
--           [0f32, 0f32, 1f32, 1f32, 1f32],
--           [0f32, 0f32, 0f32, 1f32, 1f32],
--           [0f32, 0f32, 0f32, 0f32, 1f32]]
--        }

def f (xs: []f32) = scan (+) 0 xs

entry fwd_vec (xs: []f32) : [][]f32 =
  let seeds =
    map (\i -> map (\j -> f32.bool (i == j)) (indices xs)) (indices xs)
  in (jvp2_vec f xs seeds).1

entry fwd_map (xs: []f32) : [][]f32 =
  map (\i -> jvp f xs (map (\j -> f32.bool (i == j)) (indices xs)))
      (indices xs)

entry rev_map (xs: []f32) : [][]f32 =
  map (\i -> vjp f xs (map (\j -> f32.bool (i == j)) (indices xs)))
      (indices xs)
  |> transpose

entry rev_vec (xs: []f32) : [][]f32 =
  let seeds = map (\i -> map (\j -> f32.bool (i == j)) (indices xs)) (indices xs)
  in vjp_vec f xs seeds
     |> transpose
