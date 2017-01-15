-- ==
-- error:
fun main(): i32 =
    let n = 10
    let a = iota(n)
    let (i,j) = (2,5)
    let (c, a) = (let a[i] = 0 in 1, a[i]) in -- Error: consumes and observes a in same sequence.
    5 -- Bad.
