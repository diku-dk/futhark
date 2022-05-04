def f x =
  let vs = [x, x*x, x*x*x]
  in scatter (replicate 5 1) [0,1,2] vs

-- ==
-- entry: f_jvp 
-- compiled input { 5 } output { [1, 10, 75, 0, 0] }
entry f_jvp x = jvp f x 1
