-- Test that aliasing is found, even if hidden inside a
-- branch.
-- ==
-- error:

fun main(): int =
  let n = 10 in
  let a = iota(n) in
  let c = if 2=2 then iota(n) else a in -- c aliases a.
  let c[0] = 4 in -- Consume c and a.
  a[0] -- Error, because a was consumed.
