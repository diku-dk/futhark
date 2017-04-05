-- Test that a variable shape annotation is actually bound.
-- ==
-- input {
--   [42,1337,5,4,3,2,1]
-- }
-- output {
--   [49,1344,12,11,10,9,8]
-- }

let main(a: [#n]i32): []i32 =
  map (+n) a
