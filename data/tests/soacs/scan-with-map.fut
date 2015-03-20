// This one is tricky to get to run without too many memory copies.
// When it was added, in-place-lowering couldn't get it to work right.

fun [[int]] main([int] a, [[int]] contribs) =
  scan( fn [int] ([int] x, [int] y) => zipWith(+, x, y)
      , a
      , contribs
      )
