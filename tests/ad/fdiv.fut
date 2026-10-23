-- ==
-- tags { autodiff }
-- entry: f_jvp f_vjp
-- input { 5.0f32 7.0f32 }
-- output { 0.14285f32 -0.102041f32 }

def f (x, y) = x / y : f32

entry f_jvp x y =
  ( jvp f (x, y) (1, 0)
  , jvp f (x, y) (0, 1)
  )

entry f_vjp x y =
  vjp f (x, y) 1
