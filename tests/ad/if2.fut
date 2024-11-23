-- ==
-- entry: f_jvp
-- input { [1.0,2.0,3.0] }
-- output { [0.0, 3.0, 2.0] }
-- input { [-1.0,2.0,3.0] }
-- output { [3.0, 0.0, -1.0] }
-- structure { If/Replicate 0 }

def f x : f64 =
  #[unsafe]
  let z = if x[0] < 0 then x[0] else x[1]
  let y = x[2]
  in y * z

entry f_jvp x =
  map (\i -> jvp f x (map (const 0) x with [i] = 1))
      (indices x)

-- ==
-- entry: f_vjp
-- input { [1.0,2.0,3.0] }
-- output { [0.0, 3.0, 2.0] }
-- input { [-1.0,2.0,3.0] }
-- output { [3.0, 0.0, -1.0] }

entry f_vjp x =
  vjp f x 1
