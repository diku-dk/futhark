-- ==
-- input { 2 2 [1,2,3,4] }
-- output { [[1,3],[2,4]] }

let unpack_2d n m = unflatten (i64.i32 n) (i64.i32 m)

let main n m xs: [][]i32 = unpack_2d n m xs |> transpose
