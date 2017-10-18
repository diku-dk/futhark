-- ==
-- error:
let main(): i32 =
    let n = 10
    let a = iota(n)
    let b = iota(n)
    let i = 0 in
    (let b=a in b[i]) + (let a[i]=b[i] in a[i]) -- Bad because of parallel consume-observe collision.
