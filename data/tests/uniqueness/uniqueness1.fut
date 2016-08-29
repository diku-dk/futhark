-- Test that simple function argument consumption works.
-- ==
-- input {
-- }
-- output {
--   0
-- }

fun f(a: *[]int): int = a[0]

fun main(): int =
    let n = 10
    let b = iota(n)
    let a = b -- Alias a to b.
    let x = f(b) in -- Consumes both b and a because a is aliased to b.
    0 -- OK!
