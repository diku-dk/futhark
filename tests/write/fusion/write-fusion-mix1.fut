-- Test that map-write fusion and write-write fusion work together.
-- ==
-- input {
--   [0, 1, 3]
--   [3, 2, 4, 6, 9, 14]
--   [13, 12, 14, 16, 19, 114]
-- }
-- output {
--   [3, 3, 4, 6, 6, 14]
--   [13, 12, 4, 5, 19, 7]
-- }
-- structure { Write 1 }

let main(numbers: [k]i32,
       array0: *[n]i32,
       array1: *[n]i32): ([n]i32, [n]i32) =
  let indexes0 = map (+1) numbers
  let indexes1 = map (+2) numbers
  let values0 = map (+3) numbers
  let values1 = map (+4) numbers
  let array0' = write indexes0 values0 array0
  let array1' = write indexes1 values1 array1
  in (array0', array1')
