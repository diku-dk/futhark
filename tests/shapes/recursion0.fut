-- Size-polymorphic recursion.
-- ==
-- input { [1,2,3] }
-- output { 6 }

def sum [n] (xs: [n]i32) =
  if n == 0
  then 0
  else xs[0] + sum (tail xs)

entry main = sum
