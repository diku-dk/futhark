-- Test gen_reduce on array of tuples
-- ==

let bucket_function (x : i32) : (i32, (i32, i32)) =
  (x, (1, 2))

let operator ((x0, y0) : (i32, i32)) ((x1, y1) : (i32, i32)) : (i32, i32) =
  (x0 + x1, y0 + y1)

let main [m][n] (xs : *[m](i32, i32)) (image : [n]i32) : ([m]i32, [m]i32) =
  let (is, vs) = unzip (map bucket_function image)
  in unzip (gen_reduce xs operator (1,1) is vs)
