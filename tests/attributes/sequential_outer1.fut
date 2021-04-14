-- ==
-- random input { [10][10][10]i32 } auto output
-- structure distributed {
--   /SegMap 1
--   /SegRed 0
-- }

let main xsss =
  #[incremental_flattening(only_inner)]
  map (\xss -> #[sequential_outer] map i32.sum xss) xsss
