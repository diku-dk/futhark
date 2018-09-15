-- Test
-- ==
-- input {
--   [[1, 2, 3], [1, 2, 3], [1, 2, 3]]
--   [1, 1, 1]
-- }
-- output { true }

let hist_equiv [n] (xs : [n][3]i32) (image : []i32) : [n][3]i32 =
  let inds = image
  let vals = replicate (length image) [1,2,3]
  let vals' = transpose vals
  let xs' = transpose xs
  let res = map2 (\row x -> reduce_by_index (copy x) (+) 0 inds row) vals' xs'
  in transpose res

let oned_equal [m] (xs : [m]i32) (ys : [m]i32) : bool =
  reduce (&&) true (map2 (==) xs ys)

let main [n] (xs : [n][3]i32) (image : []i32) = -- : *[n][3]i32 =
  let res1 = reduce_by_index (copy xs) (\x y -> map2 (+) x y) [0,0,0] image (replicate (length image) [1,2,3])
  let res2 = hist_equiv (copy xs) image
  in reduce (&&) true (map2 oned_equal res1 res2)
