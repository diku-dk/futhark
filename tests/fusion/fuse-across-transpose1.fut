-- ==
-- structure { Map 2 }
fun main(a: [][n]i32): [][]i32 =
  let b = map (\(x1: []i32): [n]i32  -> map (+1) x1) a
  let c = map (\(z1: []i32): [n]i32  -> map (*3) z1) (transpose(b)) in
  c
