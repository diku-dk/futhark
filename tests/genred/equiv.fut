-- Test
-- ==
-- input {
--   [[1, 2, 3], [1, 2, 3], [1, 2, 3]]
--   [1, 1, 1]
-- }
-- output { true }

let hist_equiv [n] (xs : [n][3]i32) (image : []i32) : [n][3]i32 =
  let (inds, vals) = unzip (map (\x -> (x, [1,2,3])) image)
  let vals' = transpose vals
  let xs' = transpose xs
  let res = map2 (\row x -> gen_reduce (copy x) (+) 1 inds row) vals' xs'
  in transpose res

let oned_equal [m] (xs : [m]i32) (ys : [m]i32) : bool =
  reduce (&&) true (map2 (==) xs ys)

let main [n] (xs : [n][3]i32) (image : []i32) : bool = -- : *[n][3]i32 =
  let res2 = hist_equiv (copy xs) image
  let res1 = gen_reduce (copy xs) (\x y -> map2 (+) x y) [1,1,1] image (replicate (length image) [1,2,3])
  in reduce (&&) true (map2 oned_equal res1 res2)
