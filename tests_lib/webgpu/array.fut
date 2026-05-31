-- small library used to test the javascript webgpu array wrapper

entry sum_i32_1d (xs: []i32): i32 =
  i32.sum xs

entry first_i64_2d (xss: [][]i64): i64 =
  xss[0, 0]

entry replicate_f32_1d (n: i64) (x: f32): []f32 =
  replicate n x

entry replicate_f32_2d (n: i64) (m: i64) (x: f32): [][]f32 =
  replicate n (replicate m x)