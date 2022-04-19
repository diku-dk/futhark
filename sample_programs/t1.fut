entry main (k: i64) : ([]i64) =
	let a = iota 15
	let b = map (+1) a
	let c = map (+k) b
	let d = map (+1) c
	in d
