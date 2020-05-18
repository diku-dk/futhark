-- ==
-- random input { [524288]i32 [524288]i32 } auto output

let main (input1:[]i32) (input2:[]i32) =
    let arr = map (*2) input1
    let arr' = scan (+) 0 input2
    in (arr, arr')
