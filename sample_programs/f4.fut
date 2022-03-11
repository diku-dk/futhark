
entry main (k : i64) : ([]i64, []i64) =
  let x = iota 15
  let y = map (+k) x
  let a = map (\z -> k + z - 2 + k + 15 ) x
  let z = map (\z -> z + a[z + 2 % 15]) y
  in (z,a)

-- plz no delete - i think this one fuses rly well into 1 statment
