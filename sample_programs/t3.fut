

entry main (k: i64) : ([]i64, []i64) =
	let a = iota 15
  let b = map (+1) <| iota 15

	let (a1,b1) = unzip <| map2 (\x y -> (x + k, y + k)) a b

	in (a1, b1)
