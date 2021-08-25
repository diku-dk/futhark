let pow_list [n] y (xs :[n]i32) = loop accs = (replicate n 1) for _i < y do
				   map2 (*) accs xs
					
-- ==
-- entry: prim
-- compiled input { 3 [1,2,3] } output { [1,8,27] }
entry prim y xs = pow_list y xs
		     
-- ==
-- entry: f_vjp
-- compiled input { 3 [1,2,3] } output { [3,12,27] }
entry f_vjp [n] y (xs :[n]i32) = vjp (pow_list y) xs (replicate n 1)
