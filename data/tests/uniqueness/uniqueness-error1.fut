-- Test whether multiple references within the same sequence are
-- detected.
-- ==
-- error:

fun int main() =
    let n = 10 in
    let a = copy(iota(n)) in
    let b = copy(iota(n)) in
    let {i,j} = {2,5} in
    (let a[i]=b[j] in 1) + (let b[j]=a[i] in 2) -- Error!
