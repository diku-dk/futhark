-- A recursive higher-order function used from two call sites with
-- different function arguments. Each call site must get its own
-- specialisation; they must not share the memoised lifting.
-- ==
-- input { 4i64 2i64 } output { 38i64 }

def repeat (n: i64) (f: i64 -> i64) (x: i64) : i64 =
  if n == 0 then x else repeat (n - 1) f (f x)

entry main (n: i64) (x: i64) =
  repeat n (\y -> y + 1) x + repeat n (\y -> y * 2) x
