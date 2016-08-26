-- Test that you cannot consume free variables in a loop.
-- ==
-- error:

fun main(): int =
  let n = 10 in
  let a = iota(n) in
  let b = iota(n) in
  loop (b) = for i < n do
               let a[i] = i in -- Error, because a is free and
                               -- should not be consumed.
               b in
  0
