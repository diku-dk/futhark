-- ==
-- entry: rev_J
-- no_oclgrind input {
--   [[1.0,2.0],[3.0,4.0]]
--   [[5.0,6.0],[7.0,8.0]]
--   [[1.0,2.0],[3.0,4.0]]
-- }
-- output {
--   [[17.0, 23.0], [39.0, 53.0]]
--   [[10.0, 14.0], [14.0, 20.0]]
-- }

def dotprod xs ys = f64.sum (map2 (*) xs ys)

def matmul [m] [n] [q] (xss: [m][q]f64, yss: [q][n]f64) =
  map (\xs -> map (dotprod xs) (transpose yss)) xss

entry rev_J [n] [m] [q] (xss: [m][q]f64) (yss: [q][n]f64) (res_adj: [m][n]f64) =
  vjp matmul (xss, yss) res_adj
