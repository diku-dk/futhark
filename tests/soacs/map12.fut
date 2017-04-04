-- Test map in which the map function consumes its argument.
-- ==
-- input {
--   [[1,2,3], [4,5,6]]
--   1
--   1337
-- }
-- output {
--   [[1,1337,3], [4,1337,6]]
-- }
let main(a: *[][]i32, i: i32, x: i32): *[][]i32 =
  map (\(r: *[]i32): *[]i32  ->
        let r[i] = x in r) a
