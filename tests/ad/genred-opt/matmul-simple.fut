-- ==
-- tags { autodiff no_webgpu }
-- entry: rev_J
-- no_oclgrind input {
--   [[1.0f32,2.0f32],[3.0f32,4.0f32]]
--   [[5.0f32,6.0f32],[7.0f32,8.0f32]]
--   [[1.0f32,2.0f32],[3.0f32,4.0f32]]
-- }
-- output {
--   [[17.0f32, 23.0f32], [39.0f32, 53.0f32]]
--   [[10.0f32, 14.0f32], [14.0f32, 20.0f32]]
-- }

def dotprod xs ys = f32.sum (map2 (*) xs ys)

def matmul [m] [n] [q] (xss: [m][q]f32, yss: [q][n]f32) =
  map (\xs -> map (dotprod xs) (transpose yss)) xss

entry rev_J [n] [m] [q] (xss: [m][q]f32) (yss: [q][n]f32) (res_adj: [m][n]f32) =
  vjp matmul (xss, yss) res_adj
