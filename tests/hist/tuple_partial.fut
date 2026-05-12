-- Test reduce_by_index on array of tuples where part of the tuple is not
-- recomputed.
-- ==
-- input {
--   5i64
--   [1, 3, 1]
--   [4, 1, 3]
--   [5, 6, 7]
-- }
-- output {
--   [-1, 3, -1, 1, -1]
--   [-1, 7, -1, 6, -1]
-- }

-- This is 'min', but with auxiliary information carried along with the result.
def operator ((x0, y0): (i32, i32)) ((x1, y1): (i32, i32)) : (i32, i32) =
  if x0 != -1 && (x1 == -1 || x0 < x1)
  then (x0, y0)
  else (x1, y1)

def main [n] (m: i64) (is: [n]i32) (vs0: [n]i32) (vs1: [n]i32) : ([m]i32, [m]i32) =
  let ne = (-1, -1)
  let dest = replicate m ne
  let vs = zip vs0 vs1
  in unzip (reduce_by_index dest operator ne (map i64.i32 is) vs)
