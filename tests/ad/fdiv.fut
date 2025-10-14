-- ==
-- tags { autodiff }
-- entry: f_jvp f_vjp
-- input { 5.0 7.0 }
-- output { 0.14285 -0.102041 }

def f (x, y) = x / y : f64

entry f_jvp x y =
  ( jvp f (x, y) (1, 0)
  , jvp f (x, y) (0, 1)
  )

entry f_vjp x y =
  vjp f (x, y) 1
