-- ==
-- structure { Map 2 }
fun main(a: [][n]int): [][]int =
  let b = map (\(x1: []int): [n]int  -> map (+1) x1) a
  let c = map (\(z1: []int): [n]int  -> map (*3) z1) (transpose(b)) in
  c
