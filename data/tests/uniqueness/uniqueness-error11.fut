-- Make sure occurences are checked inside function parameters as well.
-- ==
-- error:

fun int f(int x) = x

fun int main() =
    let n = 10 in
    let a = copy(iota(n)) in
    let b = copy(iota(n)) in
    let {i,j} = {2,5} in
    f((let a[i]=b[j] in 1) + (let b[j]=a[i] in 2))
