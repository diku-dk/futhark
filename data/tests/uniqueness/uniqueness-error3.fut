-- ==
-- error:
fun main(): int =
    let n = 10 in
    let a = iota(n) in
    let b = a in -- b and a alias each other.
    let (i,j) = (2,5) in
    (let a[i]=b[j] in a[i]) -- Consume a, and thus also b.
    + b[j] -- Error!
