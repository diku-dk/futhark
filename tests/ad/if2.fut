-- ==
-- tags { autodiff }
-- entry: f_jvp
-- input { [1.0f32,2.0f32,3.0f32] }
-- output { [0.0f32, 3.0f32, 2.0f32] }
-- input { [-1.0f32,2.0f32,3.0f32] }
-- output { [3.0f32, 0.0f32, -1.0f32] }
-- structure { If/Replicate 0 }

def f x : f32 =
  #[unsafe]
  let z = if x[0] < 0 then x[0] else x[1]
  let y = x[2]
  in y * z

entry f_jvp x =
  map (\i -> jvp f x (map (const 0) x with [i] = 1))
      (indices x)

-- ==
-- entry: f_vjp
-- input { [1.0f32,2.0f32,3.0f32] }
-- output { [0.0f32, 3.0f32, 2.0f32] }
-- input { [-1.0f32,2.0f32,3.0f32] }
-- output { [3.0f32, 0.0f32, -1.0f32] }

entry f_vjp x =
  vjp f x 1
