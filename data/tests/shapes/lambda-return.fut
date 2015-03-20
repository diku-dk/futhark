// Shape annotation in lambda return type.
//
// This is intended to avoid shape slices.

fun [int] multiply([int] a, int n) =
  if n == 1 then a else multiply(concat(a,a), n-1)

fun [[int]] main([[int]] a, int x) =
  let n = size(0,a) * (2 pow (x-1))
  in map(fn [int,!n] ([int] r) =>
           multiply(r,x),
         a)
