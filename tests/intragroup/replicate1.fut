-- Replication of array.
-- ==
-- compiled random input { [1][256]i32 } auto output
-- compiled random input { [100][256]i32 } auto output
-- compiled random input { [100][512]i32 } auto output

let main = map (scan (+) 0i32) >-> map (replicate 20)
