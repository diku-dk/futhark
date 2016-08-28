-- While-loop with a condition that consumes something that it has allocated itself.
-- ==
-- input {
--   [5,4,2,8,1,9,9]
--   4
-- }
-- output {
--   [5, 4, 2, 8, 6, 9, 9]
-- }

fun pointlessly_consume(x: int, a: *[]int): bool =
  x < reduce((+), 0, a)

fun main(a: *[]int, i: int): []int =
  loop (a) = while pointlessly_consume(a[i], copy(iota(i))) do
    let a[i] = a[i] + 1 in
    a in
  a
