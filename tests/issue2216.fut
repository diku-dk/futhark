-- ==
-- tags { autodiff no_webgpu }
-- input { [0.0f32, 0.0f32, 0.0f32] }
-- output { [[0.0f32, 0.0f32, 0.0f32],
--           [0.0f32, 2.0f32, 0.0f32],
--           [0.0f32, 0.0f32, 2.0f32]] }

def identity_mat n = tabulate_2d n n (\i j -> f32.bool (i == j))

def Jacobi [n] (f: [n]f32 -> [n]f32) (x: [n]f32) : [n][n]f32 =
  map (\i -> jvp f x i) (identity_mat n)

def Hessian [n] (f: [n]f32 -> f32) (x: [n]f32) : [n][n]f32 =
  Jacobi (\x -> vjp f x 1) x

entry main (x: [3]f32) = Hessian (\x -> x[1] ** 2 + x[2] ** 2) x
