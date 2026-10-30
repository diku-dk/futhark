-- ==
-- tags { autodiff }

-- ==
-- entry: f_jvp
-- input { false 5.0f32 }
-- output { 2.0f32 }
-- input { true 5.0f32 }
-- output { 11.0f32 }

def f (b, x) : f32 =
  let y = if b then x * x else x
  let z = y + x
  in z

entry f_jvp b x =
  (jvp f (b, x) (b, 1))

-- ==
-- entry: f_vjp
-- input { false 5.0f32 }
-- output { false 2.0f32 }
-- input { true 5.0f32 }
-- output { false 11.0f32 }

entry f_vjp b x =
  vjp f (b, x) 1
