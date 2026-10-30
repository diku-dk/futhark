-- Test simple indexing of a 2D array where the element type is not 32 bits.
-- ==
-- input {
--   [[4i8,3i8],[3i8,2i8],[2i8,1i8],[1i8,0i8]]
--   1
-- }
-- output {
--   [3i8,2i8]
-- }

def main (a: [][]i8) (i: i32) : []i8 =
  a[i]
