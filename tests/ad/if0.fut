-- ==
-- tags { autodiff }

-- ==
-- entry: f_jvp
-- input { true 5.0f32 7.0f32 }
-- output { 7.0f32 5.0f32 }
-- input { false 5.0f32 7.0f32 }
-- output { 0.14285f32 -0.102041f32 }

def f (b, x, y) : f32 =
  if b then x * y else x / y

entry f_jvp b x y =
  ( jvp f (b, x, y) (b, 1, 0)
  , jvp f (b, x, y) (b, 0, 1)
  )

-- ==
-- entry: f_vjp
-- input { true 5.0f32 7.0f32 }
-- output { false 7.0f32 5.0f32 }
-- input { false 5.0f32 7.0f32 }
-- output { false 0.14285f32 -0.102041f32 }

entry f_vjp b x y =
  vjp f (b, x, y) 1
