-- Result of one reduction is used free in a map.
-- ==
-- tags { no_ispc autodiff }
-- entry: fwd rev fwd_vec
-- input { [3f32, 1f32, 5f32] } output { [-1.000000f32, -1.000000f32, -1.000000f32] }

def sumBy 'a (f: a -> f32) (xs: []a) : f32 = map f xs |> f32.sum

def f (arr: []f32) =
  let mx = f32.sum arr
  let sumShiftedExp = sumBy (\x -> x - mx) arr
  in sumShiftedExp + mx

entry fwd x = map (jvp f x) [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]

entry rev x = vjp f x 1f32

entry fwd_vec x =
  jmp f x [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]
