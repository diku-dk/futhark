let test [n] (xs: [n]i32) =
  let xs' = copy xs
  let xs'' = copy xs
  in xs' with [1] =  xs''[1]
		      
-- ==
-- entry: prim
-- compiled input { [5, 7, 9] }
-- output { [5, 7, 9] }
entry prim [n] (xs: [n]i32) = test xs
		     
-- ==
-- entry: f_vjp
-- compiled input { [5, 7, 9] }
-- output { [1, 1, 1] }
entry f_vjp [n] (xs: [n]i32) = vjp test xs (replicate n 1)
