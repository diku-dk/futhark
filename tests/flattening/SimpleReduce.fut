-- ==
-- input {
--   [ [1,2,3], [5,6,7], [8,9,10] ]
-- }
-- output {
--   [ 6, 18, 27 ]
-- }
def main (xss: [][]i32) : []i32 =
  map (\(xs: []i32) : i32 ->
         reduce (+) 0 xs)
      xss
