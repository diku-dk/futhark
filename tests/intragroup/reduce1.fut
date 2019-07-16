-- Multiple intra-group reductions.
-- ==
-- random input { [1][256]i32 } auto output
-- random input { [100][256]i32 } auto output
-- random input { [100][512]i32 } auto output

let main = map (\xs -> (i32.sum xs, i32.product xs)) >-> unzip
