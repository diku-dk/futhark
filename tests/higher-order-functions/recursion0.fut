-- Recursion in a higher order function. This cannot work in all cases because
-- we do static interpretation of higher order functions, but it can work in
-- those cases where the function does not change.
-- ==
-- input { 4i64 2i64 }
-- output { 6i64 }

def repeat (n: i64) (f: i64 -> i64) (x: i64) : i64 =
  if n == 0 then x else repeat (n - 1) f (f x)

entry main (n: i64) (x: i64) = repeat n (\y -> y + 1) x
