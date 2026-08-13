-- Lambda lifting produces mutually recursive higher-order functions: the
-- recursive occurrence is inside a lambda, which therefore captures the
-- function-typed parameter and is lifted to a higher-order function that calls
-- back into the recursive one. Such a lifted function is not defunctionalised
-- until it is applied, so it does not need a self static value for a binding
-- that has not been processed yet.
-- ==
-- entry: outer
-- input { 0i64 } output { 0i64 }
-- input { 4i64 } output { 94i64 }

-- ==
-- entry: nested
-- input { 0i64 } output { 0i64 }
-- input { 3i64 } output { 135i64 }

def sumrec (g: i64 -> i64) (n: i64) : i64 =
  if n == 0
  then 0
  else i64.sum (map (\x -> g x + sumrec g (n - 1)) (iota n))

-- As above, but with the recursive occurrence two lambda levels deep.
def sumrec2 (g: i64 -> i64) (n: i64) : i64 =
  if n == 0
  then 0
  else i64.sum (map (\x -> i64.sum (map (\y -> g (x + y) + sumrec2 g (n - 1)) (iota 2))) (iota n))

entry outer (n: i64) = sumrec (\x -> x + 1) n

entry nested (n: i64) = sumrec2 (\x -> x + 1) n
