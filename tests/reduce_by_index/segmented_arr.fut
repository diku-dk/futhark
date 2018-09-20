-- ==
-- input { 4 [[0,1],[1,2],[2,3]] }
-- output {
--   [[[1, 1, 1], [1, 1, 1], [0, 0, 0], [0, 0, 0]],
--    [[0, 0, 0], [1, 1, 1], [1, 1, 1], [0, 0, 0]],
--    [[0, 0, 0], [0, 0, 0], [1, 1, 1], [1, 1, 1]]]
-- }
let main (m: i32) =
  map (\xs -> reduce_by_index (replicate m (replicate 3 0)) (map2 (+)) (replicate 3 0) xs (map (const (replicate 3 1)) xs))
