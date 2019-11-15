-- Test
-- ==
-- input {
--   [[1, 2, 3], [1, 2, 3], [1, 2, 3]]
--   [1, 1, 1]
-- }
-- output {
--   [[1i32, 2i32, 3i32], [4i32, 8i32, 12i32], [1i32, 2i32, 3i32]]
--   [[1i32, 2i32, 3i32], [4i32, 8i32, 12i32], [1i32, 2i32, 3i32]]
-- }

let hist_equiv [n][k] (xs : [n][3]i32) (image : [k]i32) : [n][3]i32 =
  let inds = image
  let vals = replicate k [1,2,3]
  let vals' = transpose vals
  let xs' = transpose xs
  let res = map2 (\row x -> reduce_by_index (copy x) (+) 0 inds row) vals' xs'
  in transpose res

let main [n][k] (xs : [n][3]i32) (image : [k]i32) = -- : *[n][3]i32 =
  let res1 = reduce_by_index (copy xs) (\x y -> map2 (+) x y) [0,0,0] image (replicate k [1,2,3])
  let res2 = hist_equiv (copy xs) image
  in (res1, res2)
