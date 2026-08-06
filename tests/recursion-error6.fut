-- As recursion-error5.fut, but where the result merely contains a function
-- rather than being one, so it cannot be written as further parameters at all.
-- ==
-- error: not first-order

def counter (n: i64) : (i64 -> i64, i64) =
  (\x -> if n == 0 then x else (counter (n - 1)).0 (x + 1), n)
