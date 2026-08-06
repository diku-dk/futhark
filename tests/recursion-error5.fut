-- The result of a recursive function must be first-order. Here it is a
-- function, which must be written as a further parameter instead.
-- ==
-- error: not first-order

def add (n: i64) : i64 -> i64 =
  \x -> if n == 0 then x else add (n - 1) (x + 1)
