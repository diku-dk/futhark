-- Don't let occurences clash just because they're function arguments.
-- ==
-- error:

fun [int] f(*[int] a) = a

fun ([int], [int]) main() =
    let n = 10 in
    let a = iota(n) in
    let b = iota(n) in
    let (i,j) = (2,5) in
    (f(let a[i]=b[j] in a),f(let b[j]=a[i] in b))
