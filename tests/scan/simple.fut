-- ==
-- random input { [100]i8 } auto output
-- random input { [257]i8 } auto output
-- random input { [513]i8 } auto output
-- random input { [10000]i8 } auto output
-- random input { [100000]i8 } auto output
-- random input { [1000000]i8 } auto output

let main x:[]i8 =
    scan (+) 0 x
