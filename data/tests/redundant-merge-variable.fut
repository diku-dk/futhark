-- Test that we can remove an unused loop result as well as the
-- computation that creates it.
--
-- ==
-- structure { DoLoop/Negate 0 }

fun [int] main(*[int] a, *[int] b, int n) =
  loop ((a,b)) = for i < n do
    let a[i] = a[i] + 1 in
    let b[i] = -b[i] in
    (a,b) in
  a
