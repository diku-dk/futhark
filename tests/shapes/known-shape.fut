-- An existing variable can be used as a shape declaration.
-- ==
-- input {
--   5
--   4
--   8
-- }
-- output {
--   [[6, 7, 8, 9, 10, 11, 12, 13],
--    [7, 8, 9, 10, 11, 12, 13, 14],
--    [8, 9, 10, 11, 12, 13, 14, 15],
--    [9, 10, 11, 12, 13, 14, 15, 16],
--    [10, 11, 12, 13, 14, 15, 16, 17]]
-- }

fun main(n: int, m: int, k: int): [n][k]int =
  let a = replicate n (iota m) in
  map (\(i: int) (r: [m]int): [k]int  ->
            let x = reduce (+) 0 r
            in map (+i) (map (+x) (iota(k)))) (
          iota(n)) a
