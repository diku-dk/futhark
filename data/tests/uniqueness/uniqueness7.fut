-- ==
-- input {
-- }
-- output {
--   0
-- }
fun f(a: *[][]int): int = a[0,0]

fun main(): int =
    let n = 10 in
    let a = copy(replicate n (iota n)) in
    let b = copy(replicate n (iota n)) in
    loop (a) = for i < n do
                 let a[i] = b[i] in a -- Does not alias a to b, because let-with is in-place!
               in
    let x = f(b) in -- Consumes only b.
    a[0,0] -- OK
