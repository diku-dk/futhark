-- Test simple indexing of an array.
-- ==
-- input {
--   [4,3,2,1,0]
--   1
-- }
-- output {
--   3
-- }

def main (a: []i32) (i: i32) : i32 =
  a[i]
