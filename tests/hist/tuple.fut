-- Test reduce_by_index on array of tuples
-- ==

def bucket_function (x: i32) : (i64, (i32, i32)) =
  (i64.i32 x, (1, 2))

def operator ((x0, y0): (i32, i32)) ((x1, y1): (i32, i32)) : (i32, i32) =
  (x0 + x1, y0 + y1)

def main [m] [n] (xs: *[m](i32, i32)) (image: [n]i32) : ([m]i32, [m]i32) =
  let (is, vs) = unzip (map bucket_function image)
  in unzip (reduce_by_index xs operator (1, 1) is vs)
