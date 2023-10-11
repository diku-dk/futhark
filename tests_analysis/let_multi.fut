-- Tests the handling of multiple patterns in let expressions
entry main [n] (xs: [n]i64) (yss: [n][n]i64) =
    let f arr0 arr1 =
        reduce_comm
        (\(z,w) (x,y) -> (z+x*y,w+x+y))
        (0,0)
        (zip arr0 arr1)
    in map (\ys ->
                #[unsafe]
                let (i,j) = f xs ys
                in xs[i]+ys[j]
        ) yss
