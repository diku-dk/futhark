-- ==
-- random input { [100]i32 } auto output
-- random input { [257]i32 } auto output
-- random input { [513]i32 } auto output

let main x:[]i32 =
    scan (+) 0 x
