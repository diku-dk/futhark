-- ==
-- tags { autodiff }
-- input { [1.0f32, 1.0f32, 1.0f32] } output { [-4.0f32, -2.0f32, 0.0f32] }

entry main seed =
  let eq (w': [3]f32) =
    let w0 = 1 - 2 * f32.sum w'
    let w = concat [w0] w'
    let power n x = iterate (i32.i64 n) (* x) 1
    let A =
      tabulate_2d 4 4 (\i j -> (1 + f32.bool (j != 0)) * power i w[j])
      |> map (scan (+) 0)
    in A[1, 1:]
  let start_w = [0.1027, -1.9606, 1.9381]
  in jvp eq start_w seed
