-- ==
-- entry: f_vjp
-- input { [[1,2,3],[1,2,3],[1,2,3]] [1,2,3]}
-- output { [[1,1,1],[1,1,1],[1,1,1]] [3,3,3] }
def f [n] (xssys: ([n][n]i32, [n]i32)) =
  let (xss, ys) = xssys
  in i32.sum (map (\xs -> i32.sum (map2 (*) xs ys)) xss)

entry f_vjp [n] (xss: [n][n]i32) (ys: [n]i32) =
  vjp (\(xss, ys) -> vjp f (xss, ys) 1) (xss, ys) (replicate n (replicate n 1), replicate n 1)
