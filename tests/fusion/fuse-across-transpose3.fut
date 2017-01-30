-- ==
-- structure { Redomap 2 }
fun main(a: [n][m]i32): i32 =
  let b = map (\(z1: []i32): [m]i32  ->
                map (*3) z1) a
  let ravgs = map (\(r: []i32): i32  ->
                    reduce (+) 0 r / n) (
                  transpose(b))
  let res = reduce (+) 0 ravgs in
  res
