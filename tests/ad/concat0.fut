-- ==
-- entry: f_jvp
-- compiled input { [1,2,3] [4,5,6] }
-- output { [1,2,3,4,5,6] }

entry f_jvp xs ys : []i32 =
 jvp (uncurry concat) (xs,ys) (xs, ys)

-- ==
-- entry: f_vjp
-- compiled input { [1,2,3] [4,5,6] }
-- output { [1,2,3] [4,5,6] }

entry f_vjp xs ys : ([]i32, []i32) =
 vjp (uncurry concat) (xs,ys) (concat xs ys)
