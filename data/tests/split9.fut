// Checks that the results of splits are properly copied to where they
// are supposed to go.

fun [int] take(int n, [int] r) =
  let {part, _} = split( (n), r) in
  part

fun [[int]] main([[int]] rs, int n) =
  map(take(n), rs)
