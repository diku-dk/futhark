-- Some simple currying of operators.
-- ==
-- input {
--   [-3,-2,-1,0,1,2,3]
-- }
-- output {
--   [-2, -1, -1, 0, 0, 1, 1]
--   [5, 4, 3, 2, 1, 0, -1]
-- }

def main (a: []i32) : ([]i32, []i32) =
  (map (/ 2) a, map (2 -) a)
