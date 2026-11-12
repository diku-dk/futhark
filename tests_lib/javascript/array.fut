-- Entry points that accept and return arrays.

entry sum_i32_1d (xs: []i32) : i32 = i32.sum xs

entry sum_i64_2d (xss: [][]i64) : i64 = i64.sum (map i64.sum xss)

entry replicate_f32_1d (n: i64) (x: f32) : []f32 = replicate n x

entry replicate_f32_2d (n: i64) (m: i64) (x: f32) : [][]f32 =
  replicate n (replicate m x)

entry iota_i32_3d (a: i64) (b: i64) (c: i64) : [][][]i32 =
  tabulate_3d a b c (\i j k -> i32.i64 (i * b * c + j * c + k))

entry halve_f16_1d (xs: []f16) : []f16 = map (/ 2) xs

entry not_bool_1d (xs: []bool) : []bool = map (\x -> !x) xs

-- Entry points that consume their argument.
entry consume_i32_1d (xs: *[]i32) : []i32 = xs with [0] = 42

entry consume_first (xs: *[]i32) (ys: []i32) : i32 =
  i32.sum (xs with [0] = 0) + i32.sum ys

-- Entry points with two array arguments, which may be the same array.
entry add_i32_1d (xs: []i32) (ys: []i32) : []i32 = map2 (+) xs ys

entry and_bool_1d (xs: []bool) (ys: []bool) : []bool = map2 (&&) xs ys
