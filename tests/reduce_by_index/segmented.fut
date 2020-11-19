-- ==
-- input { 10i64 [[1,2,3],[2,3,4],[3,4,5]] }
-- output {
-- [[0i32, 1i32, 1i32, 1i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32],
--  [0i32, 0i32, 1i32, 1i32, 1i32, 0i32, 0i32, 0i32, 0i32, 0i32],
--  [0i32, 0i32, 0i32, 1i32, 1i32, 1i32, 0i32, 0i32, 0i32, 0i32]]
-- }

let main (m: i64) =
  map (\xs -> reduce_by_index (replicate m 0) (+) 0
                              (map i64.i32 xs)
                              (map (const 1) xs))
