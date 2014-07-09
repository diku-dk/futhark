// Test that we cannot consume anything inside an anonymous function.

fun int f(*[int] a) = a[0]

fun int main(int n) =
  let a = copy(iota(n)) in
  reduce(fn int (int sum, int i) => sum + f(a), 0, iota(10))
