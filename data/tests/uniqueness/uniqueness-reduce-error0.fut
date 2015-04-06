// If the reduction function accumulator type is unique, consume the
// initial value.
// --
// error:

fun [int] main(*[int] a) =
  let b = reduce(fn *[int] (*[int] acc, [int] i) => acc, a, replicate(10,iota(10))) in
  map(op+, zip(a, b)) // Should fail, because a has been consumed!
