-- Simplest possible in-place operation.
-- ==
-- input {
--   [1,2,3,4]
--   2
--   10
-- }
-- output {
--   [1,2,10,4]
-- }
def main (a: *[]i32) (i: i32) (x: i32) : []i32 =
  let a[i] = x
  in a
