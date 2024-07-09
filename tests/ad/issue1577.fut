-- ==
-- entry: main
-- compiled input { [1i64,1i64,3i64,3i64] [1,2,3,4] }
-- output { [0,3,0,7,0] }

let red [n] (is: [n]i64) (vs: [n]i32) =
     reduce_by_index (replicate 5 1) (*) 1 is vs

let main [n] (is: [n]i64) (vs: [n]i32) =
    jvp (red is) vs (replicate n 1)
