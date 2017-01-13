-- ==
-- structure { Redomap 2 }
fun main(a: [n][m]int): int =
  let b = map (\(z1: []int): [m]int  ->
                map (*3) z1) a
  let ravgs = map (\(r: []int): int  ->
                    reduce (+) 0 r / n) (
                  transpose(b))
  let res = reduce (+) 0 ravgs in
  res
