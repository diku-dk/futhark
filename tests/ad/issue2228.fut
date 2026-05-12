-- ==
-- tags { autodiff }
-- input { [[2.0,3.0]] [1.0,2.0] [3.0,4.0] 5.0 [1.0,2.0] [3.0,5.0] true }
-- output { -16.0 }

def grad f x = vjp f x 1f64

def magnitude_squared = map (** 2) >-> f64.sum

def distance_squared u v = magnitude_squared (map2 (-) u v)

def naive_euler charges x_initial xdot_initial (w: f64) =
  let charges = map (map (+ w)) charges
  let p x = f64.sum (map (distance_squared x) charges)
  in loop (x: [2]f64, xdot: [2]f64, go) = (x_initial, xdot_initial, true)
     while go do
       let xddot = grad p x
       in if xdot[1] > 0
          then (xdot, xddot, true)
          else (x, xdot, false)

entry main charges x_initial xdot_initial x a b c =
  vjp (naive_euler charges x_initial xdot_initial) x (a, b, c)
