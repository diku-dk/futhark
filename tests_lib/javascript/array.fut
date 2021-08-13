-- Library with entry points that accept and return
-- array inputs and outputs

entry sum1d (xs : []i32) : i32 = i32.sum xs

entry sum2d (xss : [][]i64) : i64 =
  let row_sums = map i64.sum xss
  in i64.sum row_sums

entry replicate1d (n : i64) (x : f32) : []f32 = replicate n x

entry replicate2d (n: i64) (m : i64) (x : f32) : [][]f32 =
  let row = replicate m x
  in replicate n row
