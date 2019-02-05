-- ==
-- input {
--   [[1,2],[3,4]]
--   [[5,6],[7,8]]
-- }
-- output {
--   [[1,2],[3,4],[5,6],[7,8]]
-- }
-- input { empty([]i32) empty([]i32) } output { empty([]i32) }
-- input { empty([]i32) [[1]] } output { [[1]] }
-- input { [[1]] empty([]i32) } output { [[1]] }

let main(a: [][]i32) (b: [][]i32): [][]i32 =
  concat a b
