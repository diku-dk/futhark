-- ==
-- tags { autodiff }

-- ==
-- entry: f_jvp
-- input { false 5.0 }
-- output { 2.0 }
-- input { true 5.0 }
-- output { 11.0 }

def f (b, x) : f64 =
  let y = if b then x * x else x
  let z = y + x
  in z

entry f_jvp b x =
  (jvp f (b, x) (b, 1))

-- ==
-- entry: f_vjp
-- input { false 5.0 }
-- output { false 2.0 }
-- input { true 5.0 }
-- output { false 11.0 }

entry f_vjp b x =
  vjp f (b, x) 1
