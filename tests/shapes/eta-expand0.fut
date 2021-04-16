-- ==
-- input { 2 2 [1,2,3,4] }
-- output { 2i64 2i64 [[1,2],[3,4]] }

let unpack_2d n m = unflatten (i64.i32 n) (i64.i32 m)

let main n m xs =
  let [x][y] (ys: [x][y]i32) = unpack_2d n m xs
  in (x,y,ys)
