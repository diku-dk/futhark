-- Simple recursive definition of fact.
-- ==
-- input { 10i64 }
-- output { 3628800i64 }

def fact (n: i64) = if n == 0 then 1 else n * fact (n - 1)

entry main = fact
