-- ==
-- tags { autodiff no_webgpu }
-- input { [[2.0f32,3.0f32]] [1.0f32,2.0f32] [3.0f32,4.0f32] 5.0f32 [1.0f32,2.0f32] [3.0f32,5.0f32] true }
-- output { -16.0f32 }

def grad f x = vjp f x 1f32

def magnitude_squared = map (** 2) >-> f32.sum

def distance_squared u v = magnitude_squared (map2 (-) u v)

def naive_euler charges x_initial xdot_initial (w: f32) =
  let charges = map (map (+ w)) charges
  let p x = f32.sum (map (distance_squared x) charges)
  in loop (x: [2]f32, xdot: [2]f32, go) = (x_initial, xdot_initial, true)
     while go do
       let xddot = grad p x
       in if xdot[1] > 0
          then (xdot, xddot, true)
          else (x, xdot, false)

entry main charges x_initial xdot_initial x a b c =
  vjp (naive_euler charges x_initial xdot_initial) x (a, b, c)
