-- Size-polymorphic recursion.
-- ==
-- input { [1,2,3] }
-- output { 9 }

def sum [n] (xs: [n]i32) =
  if n == 0
  then 0
  else if n == 1
  then xs[0]
  else xs[0] + sum (drop 1 xs) + sum (drop 2 xs)

entry main = sum
