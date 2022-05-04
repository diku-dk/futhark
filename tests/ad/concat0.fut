-- ==
-- entry: f_jvp
-- compiled input { [1,2,3] [4,5,6] }
-- output { [1,2,3,4,5,6] }

entry f_jvp xs ys : []i32 =
 let m = length xs + length ys
 in jvp (uncurry (concat_to m)) (xs,ys) (xs, ys)

-- ==
-- entry: f_vjp
-- compiled input { [1,2,3] [4,5,6] }
-- output { [1,2,3] [4,5,6] }

entry f_vjp xs ys : ([]i32, []i32) =
 let m = length xs + length ys
 in vjp (uncurry (concat_to m)) (xs,ys) (concat_to m xs ys)
