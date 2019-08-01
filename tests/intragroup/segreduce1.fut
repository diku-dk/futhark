-- Multiple intra-group reductions.
-- ==
-- random input { [1][1][256]i32 } auto output
-- random input { [10][10][256]i32 } auto output
-- random input { [10][10][512]i32 } auto output

let main = map (map (\xs -> (i32.sum xs, i32.product xs))) >-> map unzip >-> unzip
