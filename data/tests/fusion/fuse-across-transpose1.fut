-- ==
-- structure { Map 2 }
fun [][]int main([][n]int a) =
  let b = map(fn [n]int ([]int x1) => map(+1, x1), a) in
  let c = map(fn [n]int ([]int z1) => map(*3, z1), transpose(b)) in
  c
