def f xs = map (\x -> x*x) xs

-- ==
-- entry: f_jvp 
-- compiled input { [1,2,3] } output { [[2,0,0],[0,4,0],[0,0,6]] }
entry f_jvp [n] (xs: [n]i32) : [n][n]i32 = (jvp2_vec f xs (tabulate n (\i -> replicate n 0 with [i] = 1)) ).1
