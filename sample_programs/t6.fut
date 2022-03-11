
entry main (k : i64) : []i64 =
  let z' = iota 10
  let z = map (+1) z'
  let a = reduce (+) 0 z
  let b = reduce (+) 0 z
  let c =  b + a + k
  let d = map (+c) z
  let e = map (+c) d
  in e
