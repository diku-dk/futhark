entry main (k: i64) : ([]i64, []i64) =
	let a = iota 15

	let ba = map (+k) a
	let b = map (+k) ba

	let ca = map (+k) a
	let c = map (+k) ca
	
	in (b,c)
