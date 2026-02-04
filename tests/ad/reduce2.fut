-- Result of one reduction is used free in a map.
-- ==
-- tags { no_ispc autodiff }
-- entry: fwd rev
-- input { [3f64, 1f64, 5f64] } output { [-1.000000f64, -1.000000f64, -1.000000f64] }

def sumBy 'a (f: a -> f64) (xs: []a) : f64 = map f xs |> f64.sum

def f (arr: []f64) =
  let mx = f64.sum arr
  let sumShiftedExp = sumBy (\x -> x - mx) arr
  in sumShiftedExp + mx

entry fwd x = map (jvp f x) [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]

entry rev x = vjp f x 1f64
