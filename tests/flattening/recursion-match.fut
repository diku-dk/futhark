-- Test that flattening of a variant 'Match' does not execute untaken branches.
-- ==
-- tags { disable }
-- input { 4i64 } output { 4i64 }

def f (n: i64) : i64 =
  if n == 0
  then 0
  else 1 + i64.maximum (map f (iota n))

entry main = f
