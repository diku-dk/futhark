entry main (k : i64) (k1 : i64) : ([]i64, []i64, []i64) =
	let a = map (+k) (iota 10)
	let b = map (*k) a
	let c = map (\v -> v-k) a
	let d = map3 (\x y z -> (y, x, z)) b c a
	in unzip3 d
