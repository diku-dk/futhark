-- ==
-- input {
-- }
-- output {
--   [1, 2, 3]
--   true
-- }
def main : ([]i32, bool) =
  let arr = [([1, 2, 3], true), ([4, 5, 6], false)]
  in arr[0]
