-- Tests a loop that simplification once messed up royally.
-- ==
-- input { 50 1.1 }
-- output { 14 }

fun p(c: f64): bool =
  c < 4.0

fun f(x: f64, y: f64): f64 =
  x * y

fun main(depth: int, a: f64): int =
  loop ((c, i) = (a, 0)) = while i < depth && p(c) do
    (f(a, c),
     i + 1) in
  i
