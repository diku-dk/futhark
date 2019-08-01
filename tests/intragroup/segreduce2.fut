-- Map-reduce inside group.
-- ==
-- random input { [1][1][256]i32 } auto output
-- random input { [10][10][256]i32 } auto output
-- random input { [10][10][512]i32 } auto output

let main = map (map (map i32.abs >-> i32.sum))
