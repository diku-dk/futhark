-- ==
-- tags { autodiff }
-- input { [1.0, 1.0, 1.0] } output { [-4.0, -2.0, 0.0] }

entry main seed =
  let eq (w': [3]f64) =
    let w0 = 1 - 2 * f64.sum w'
    let w = concat [w0] w'
    let power n x = iterate (i32.i64 n) (* x) 1
    let A =
      tabulate_2d 4 4 (\i j -> (1 + f64.bool (j != 0)) * power i w[j])
      |> map (scan (+) 0)
    in A[1, 1:]
  let start_w = [0.1027, -1.9606, 1.9381]
  in jvp eq start_w seed
