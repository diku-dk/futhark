fun int f(*[int] ar, *[[int]] a) =
  f(a[0], a) // Should be a type error, as both are supposed to be uniqu

fun int main(int n) =
  let a = copy(replicate(n, iota(n))) in
  let ar = copy(a[0]) in
  f(ar, a)
