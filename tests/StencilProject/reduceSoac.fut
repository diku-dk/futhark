let main =
	let xs = [1i32,2,3,4,5]
	let a = map (\i -> xs[i64.max 0 (i-1)] + xs[0] + xs[i64.min (i+1) 4]) (iota 5)
	in a
