-- ==
-- random input { [524288]i32 } auto output

let main (input:[]i32) =
    let arr = map (*2) input
    let arr' = scan (+) 0 arr
    in (arr, arr')

-- input @kA-13.data auto output
-- input @kA-42.data auto output
-- input @kA-8704.data auto output
-- input @kA-32768.data auto output
-- input @kA-524288.data auto output
-- input @kA-1048576.data auto output
-- input @kA-65536.data auto output
-- input @kA-131072.data auto output
-- input @kA-262144.data auto output
-- input @kA-16777216.data auto output



