// Test that shape declarations are taken into account even when the
// function is curried.
//
// At the time this test was written, the only way to determine the
// success of this is to inspect the result of internalisation by the
// compiler.

fun [int,!n] oneToEach([int,n] r) =
  map(+1, r)

fun [[int]] main([[int]] a) =
  map(oneToEach, a)
