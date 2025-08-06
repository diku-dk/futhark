def test =
  let x = [1, 2, 3, 4, 5]
  let y = x |> map (\x -> x + x) with [0] = 10
  in y |> i64.sum |> (+ 10)
