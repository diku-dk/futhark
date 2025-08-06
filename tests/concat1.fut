-- Basic 1D concatting.
-- ==
-- input {
--   [1,2]
--   [3,4]
-- }
-- output {
--   [1,2,3,4]
-- }
-- input {
--   empty([0]i32)
--   [1,2,3]
-- }
-- output {
--   [1,2,3]
-- }

def main (a: []i32) (b: []i32) : []i32 =
  concat a b
