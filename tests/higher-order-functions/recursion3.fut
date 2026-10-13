-- A recursive function that is partially applied, so that the result is used as
-- a function value rather than immediately applied.
-- ==
-- entry: main
-- input { 0i64 7i64 } output { 7i64 }
-- input { 3i64 7i64 } output { 10i64 }

-- ==
-- entry: partial
-- input { 0i64 7i64 } output { 15i64 }
-- input { 3i64 7i64 } output { 21i64 }

def add (n: i64) (x: i64) : i64 =
  if n == 0 then x else add (n - 1) (x + 1)

entry main (n: i64) (x: i64) = add n x

entry partial (n: i64) (x: i64) =
  let f = add n
  in f x + f (x + 1)
