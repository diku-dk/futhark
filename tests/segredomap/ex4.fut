-- A segmented-redomap using the same array in both maps
-- ==
-- input {
--   [1i32, 2i32, 3i32, 4i32]
-- }
-- output {
--   [14i32, 18i32, 22i32, 26i32]
-- }
entry main [n] (xs: [n]i32) : [n]i32 =
  map (\y ->
         let zs = map (\x -> x + y) xs
         in reduce (+) 0 zs)
      xs
