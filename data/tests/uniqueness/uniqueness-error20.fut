-- Test that you can't consume a free variable in a lambda.
-- ==
-- error:

fun int main(int n) =
  let a = copy(iota(n)) in
  let b = map(fn int (int x) => let a[x] = 4 in a[x], iota(n)) in
  0
