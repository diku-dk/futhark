-- Test gen_reduce on array of arrays
-- ==
--
-- input  {}
-- output {}

let main [m][n] (xs : *[n][m]i32) (image : []i32) : *[n][m]i32 =
 gen_reduce xs (\x y -> map2 (+) x y) [1,1,1] (\x -> (x, [1,2,3])) image
