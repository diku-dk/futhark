-- ==
-- tags { autodiff no_webgpu }
-- entry: fwd_vec fwd_map rev_map rev_vec
-- input { [1.0f32, 2.0f32, 3.0f32] }
-- output { [[1.0f32, 0.0f32, 0.0f32, 1.0f32, 0.0f32, 0.0f32], [0.0f32, 1.0f32, 0.0f32, 0.0f32, 1.0f32, 0.0f32], [0.0f32, 0.0f32, 1.0f32, 0.0f32, 0.0f32, 1.0f32]] }

def f (xs: []f32) = xs ++ xs

entry fwd_vec (xs: []f32) =
  let seeds =
    map (\i -> map (\j -> f32.bool (i == j)) (indices xs)) (indices xs)
  in (jmp2 f xs seeds).1

entry fwd_map (xs: []f32) =
  map (\i -> jvp f xs (map (\j -> f32.bool (i == j)) (indices xs)))
      (indices xs)

-- One cotangent per element of the result; transposing the Jacobian then gives
-- the same shape as the forward entries produce.
def cotangents (n: i64) =
  tabulate (n + n) (\i -> replicate (n + n) 0f32 with [i] = 1)

entry rev_map [n] (xs: [n]f32) =
  transpose (map (vjp f xs) (cotangents n))

entry rev_vec [n] (xs: [n]f32) =
  transpose (mjp f xs (cotangents n))

entry rev xs ys : ([]i32, []i32) =
  vjp (uncurry concat) (xs, ys) (concat xs ys)
