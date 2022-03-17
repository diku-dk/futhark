
entry main (k : i64) : ([]i64,i64,i64,i64,[]i64) =
  let z = iota 10
  let r1 = map (+1) z
  let r2 = reduce (+) 0 z
  let r3 = scan (+) 0 z
  let r4 = reduce (+) 0 z
  let r5 = reduce (+) 0 z
  in (r1, r2, r4, r5, r3)
