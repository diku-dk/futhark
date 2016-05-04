-- Tests a loop that simplification once messed up royally.
-- ==
-- input { 50 1.1 }
-- output { 14 }

fun bool p(f64 c) =
  c < 4.0

fun f64 f(f64 x, f64 y) =
  x * y

fun int main(int depth, f64 a) =
  loop ((c, i) = (a, 0)) = while i < depth && p(c) do
    (f(a, c),
     i + 1) in
  i
