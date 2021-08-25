let mult_list xs =
  loop start = 1 for x in xs do
    x * x
			   
-- ==
-- entry: prim
-- compiled input { [11,5,13] } output { 169 }
entry prim = mult_list
	     
-- ==
-- entry: f_vjp 
-- compiled input { [11,5,13] } output { [0,0,26] }
entry f_vjp [n] (xs: [n]i32) = vjp mult_list xs 1
