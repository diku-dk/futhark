-- Test that map-write fusion and write-write fusion work together.
-- ==
-- input {
--   [2, 0]
--   [1, 0]
--   [100, 80]
--   [90, 80]
--   [0, 2, 4, 6, 9]
--   [10, 12, 14, 16, 19]
-- }
-- output {
--   [84, 2, 104, 6, 9]
--   [240, 270, 14, 16, 19]
-- }
-- structure { Write 1 }

let main(indexes0: [k]i32,
       indexes1: [k]i32,
       values0: [k]i32,
       values1: [k]i32,
       array0: *[n]i32,
       array1: *[n]i32): ([n]i32, [n]i32) =
  let values0' = map (+4) values0
  let values1' = map (*3) values1
  let array0' = write indexes0 values0' array0
  let array1' = write indexes1 values1' array1
  in (array0', array1')
