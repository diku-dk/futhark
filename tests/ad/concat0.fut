-- ==
-- tags { autodiff }
-- entry: fwd_vec fwd_map rev_map rev_vec
-- input { [1.0, 2.0, 3.0] }
-- output { [[1.0, 0.0, 0.0, 1.0, 0.0, 0.0], [0.0, 1.0, 0.0, 0.0, 1.0, 0.0], [0.0, 0.0, 1.0, 0.0, 0.0, 1.0]] }

def f (xs: []f64) = xs ++ xs

entry fwd_vec (xs: []f64) =
  let seeds =
    map (\i -> map (\j -> f64.bool (i == j)) (indices xs)) (indices xs)
  in (jmp2 f xs seeds).1

entry fwd_map (xs: []f64) =
  map (\i -> jvp f xs (map (\j -> f64.bool (i == j)) (indices xs)))
      (indices xs)

-- One cotangent per element of the result; transposing the Jacobian then gives
-- the same shape as the forward entries produce.
def cotangents (n: i64) =
  tabulate (n + n) (\i -> replicate (n + n) 0f64 with [i] = 1)

entry rev_map [n] (xs: [n]f64) =
  transpose (map (vjp f xs) (cotangents n))

entry rev_vec [n] (xs: [n]f64) =
  transpose (mjp f xs (cotangents n))

entry rev xs ys : ([]i32, []i32) =
  vjp (uncurry concat) (xs, ys) (concat xs ys)
