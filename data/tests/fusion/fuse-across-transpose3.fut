-- ==
-- structure { Redomap 2 }
fun int main([n][m]int a) =
  let b = map(fn [m]int ([]int z1) =>
                map(*3, z1),
              a) in
  let ravgs = map(fn int ([]int r) =>
                    reduce(+, 0, r) / n,
                  transpose(b)) in
  let res = reduce(+, 0, ravgs) in
  res
