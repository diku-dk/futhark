-- Test that map-write fusion works in a slightly less simple case.
-- ==
-- input {
--   [2, 0]
--   [100, 200]
--   [0, 2, 4, 6, 9]
-- }
-- output {
--   [200, 2, 102, 6, 9]
-- }
-- structure { Write 1 }

fun [n]i32
  main([k]i32 indexes,
       [k]i32 values,
       *[n]i32 array) =
  let values' = map(+, zip(indexes, values))
  let array' = write(indexes, values', array)
  in array'
