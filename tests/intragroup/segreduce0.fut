-- Simple intra-group reduction.
-- ==
-- random input { [1][1][256]i32 } auto output
-- random input { [10][10][256]i32 } auto output
-- random input { [10][10][512]i32 } auto output

let main = map (map i32.sum)
