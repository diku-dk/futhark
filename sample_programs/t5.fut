
entry main (k : i64) : ([]i64) =
  let z = iota 10
  let a = map (\x -> x + k + k) z
  let b = map (+k) a
  let c = map2 (\x _ -> x + k * 2) a b
  in map2 (+) b c
