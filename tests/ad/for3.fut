let square [n] (xs: [n]i32) =
  let xs' = copy xs
  in loop xs'' = xs' for i < n do
       let a = xs''[i]
       in xs'' with [i] = a * a
					
-- ==
-- entry: prim
-- compiled input { [1,2,3,4,5] } output { [1,4,9,16,25] }
entry prim [n] (xs: [n]i32) = square xs
		     
-- ==
-- entry: f_vjp
-- compiled input { [1,2,3,4,5] } output { [2,4,6,8,10] }
entry f_vjp [n] (xs: [n]i32) = vjp square xs (replicate n 1)
