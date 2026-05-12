-- ==
-- input {
--   [[1,2],[3,4]]
--   [[5,6],[7,8]]
-- }
-- output {
--   [[1,2],[3,4],[5,6],[7,8]]
-- }
-- input { empty([0][0]i32) empty([0][0]i32) } output { empty([0][0]i32) }
-- input { empty([0][1]i32) [[1]] } output { [[1]] }
-- input { [[1]] empty([0][1]i32) } output { [[1]] }

def main (a: [][]i32) (b: [][]i32) : [][]i32 =
  concat a b
