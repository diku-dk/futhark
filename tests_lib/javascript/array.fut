-- Entry points that accept and return arrays.

entry sum_i32_1d (xs: []i32) : i32 = i32.sum xs

entry sum_i64_2d (xss: [][]i64) : i64 = i64.sum (map i64.sum xss)

entry replicate_f32_1d (n: i64) (x: f32) : []f32 = replicate n x

entry replicate_f32_2d (n: i64) (m: i64) (x: f32) : [][]f32 =
  replicate n (replicate m x)

entry iota_i32_3d (a: i64) (b: i64) (c: i64) : [][][]i32 =
  tabulate_3d a b c (\i j k -> i32.i64 (i * b * c + j * c + k))
