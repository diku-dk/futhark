def f x = map (*(x*x)) [0,1,2]

-- ==
-- entry: f_jvp
-- compiled input { 2 } output { [0, 4, 8] }
-- compiled input { 4 } output { [0, 8, 16] }
entry f_jvp x = jvp f x 1
