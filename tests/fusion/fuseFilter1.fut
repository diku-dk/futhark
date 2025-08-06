-- ==
-- input {
--   [3,5,-2,3,4,-30]
--   [-4,10,1,-8,2,4]
-- }
-- output {
--   [1, 4]
-- }
def main (a: []i32) (b: []i32) : []i32 =
  let (c, d) = unzip (filter (\(x, y) -> x + y < 0) (zip a b))
  in filter (0 <) d
