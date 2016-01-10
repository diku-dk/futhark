-- Tests a loop that simplification once messed up royally.
-- ==
-- input { 50 1.1 }
-- output { 14 }

fun bool p(real c) =
  c < 4.0

fun real f(real x, real y) =
  x * y

fun int main(int depth, real a) =
  loop ({c, i} = {a, 0}) = while i < depth && p(c) do
    {f(a, c),
     i + 1} in
  i
