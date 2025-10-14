def f (x, y) : [2][1]f64 = [x, y]

-- ==
-- tags { autodiff }
-- entry: f_vjp f_jvp
-- input { [5.0] [7.0] }
-- output { [[1.0],[0.0]] [[0.0], [1.0]] }

entry f_jvp x y =
  ( jvp f (x, y) ([1], [0])
  , jvp f (x, y) ([0], [1])
  )

entry f_vjp x y =
  let (dx1, dx2) = vjp f (x, y) [[1], [0]]
  let (dy1, dy2) = vjp f (x, y) [[0], [1]]
  in ([dx1, dy1], [dx2, dy2])
