-- ==
-- error:
fun main(): int =
    let n = 10 in
    let a = iota(n) in
    let (i,j) = (2,5) in
    let (c, a) = (let a[i] = 0 in 1, a[i]) in -- Error: consumes and observes a in same sequence.
    5 -- Bad.
