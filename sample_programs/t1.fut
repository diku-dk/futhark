entry main (k: i64) : ([]i64) =
	let a = iota 15
	let b = map (+k) a
	let c = map (+k) b
	in c
