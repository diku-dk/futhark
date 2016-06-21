-- Test that map-write fusion works in a simple case.
-- ==
-- input {
--   [2, 0]
--   [100, 200]
--   [0, 2, 4, 6, 9]
-- }
-- output {
--   [0, 200, 4, 100, 9]
-- }
-- structure { Write 1 }

fun [n]i32
  main([k]i32 indexes,
       [k]i32 values,
       *[n]i32 array) =
  let indexes' = map(+1, indexes)
  let array' = write(indexes', values, array)
  in array'
