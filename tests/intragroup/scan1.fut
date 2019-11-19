-- Multiple intra-group scans.
-- ==
-- random input { [1][256]i32 } auto output
-- compiled random input { [100][256]i32 } auto output
-- compiled random input { [100][512]i32 } auto output

let main = map (\xs -> (scan (+) 0i32 xs, scan (*) 1i32 xs)) >-> unzip
