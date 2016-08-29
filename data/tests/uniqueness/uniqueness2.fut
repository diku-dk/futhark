-- ==
-- input {
-- }
-- output {
--   3
-- }
fun main(): int =
    let n = 10
    let (a, b) = (copy(replicate n 0), copy(replicate n 0))
    let a[0] = 1
    let b[0] = 2 in
    a[0] + b[0]
