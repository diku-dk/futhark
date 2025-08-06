-- ==
-- entry: f_vjp
-- input { [[1,2,3],[1,2,3],[1,2,3]] [0,1,2] [0,1,2]}
-- output { [[6,12,18],[6,12,18],[6,12,18]] [0,0,0] [0,0,0] }
def f [n] (xssisjs: ([n][n]i32, [n]i32, [n]i32)) =
  let (xss, is, js) = xssisjs
  in map (\i -> map (\j -> xss[i, j] * xss[i, j] * xss[i, j]) js) is

entry f_vjp [n] (xss: [n][n]i32) (is: [n]i32) (js: [n]i32) =
  vjp (\(xss, is, js) -> vjp f (xss, is, js) (replicate n (replicate n 1))) (xss, is, js) ((replicate n (replicate n 1)), replicate n 0, replicate n 0)
