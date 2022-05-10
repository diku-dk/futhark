def pow y x = loop acc = 1 for _i < y do
	        acc * x
-- ==
-- entry: prim
-- compiled input { 3 4 } output { 64 }
-- compiled input { 9 3 } output { 19683 }
entry prim y x = pow y x

-- ==
-- entry: f_jvp f_vjp 
-- compiled input { 3 4 } output { 48 }
-- compiled input { 9 3 } output { 59049 }
entry f_jvp y x = jvp (pow y) x 1
entry f_vjp y x = vjp (pow y) x 1
