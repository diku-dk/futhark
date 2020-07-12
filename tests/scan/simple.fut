-- ==
-- random input { [100]i32 } auto output
-- random input { [257]i32 } auto output
-- random input { [513]i32 } auto output
-- random input { [10000]i32 } auto output
-- random input { [100000]i32 } auto output
-- random input { [1000000]i32 } auto output

let main x =
    scan (+) 0 x
