-- ==
-- error:
fun main(): int =
    let n = 10 in
    let a = iota(n) in
    let b = iota(n) in
    let i = 0 in
    (let b=a in b[i]) + (let a[i]=b[i] in a[i]) -- Bad because of parallel consume-observe collision.
