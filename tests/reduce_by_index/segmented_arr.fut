-- ==
-- input { 4i64 [[0,1],[1,2],[2,3]] }
-- output {
--   [[[1, 1, 1], [1, 1, 1], [0, 0, 0], [0, 0, 0]],
--    [[0, 0, 0], [1, 1, 1], [1, 1, 1], [0, 0, 0]],
--    [[0, 0, 0], [0, 0, 0], [1, 1, 1], [1, 1, 1]]]
-- }
def main (m: i64) =
  map (\xs -> reduce_by_index (replicate m (replicate 3 0)) (map2 (+)) (replicate 3 0) (map i64.i32 xs) (map (const (replicate 3 1)) xs))
