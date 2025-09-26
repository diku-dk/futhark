-- More comprehensive test for size expression handling
-- Tests various complex size expressions in higher-order functions
-- ==
-- input { 2i64 3i64 } output { [1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0] [2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0] }

-- Test with power operation
def f1 (n: i64) (m: i64) (g: f64 -> [m ** n]f64) = g 1.0

-- Test with multiplication 
def f2 (n: i64) (m: i64) (g: f64 -> [m * n + 1]f64) = g 2.0

entry main n m = 
  ( f1 n m (\x -> replicate (m ** n) x),
    f2 n m (\x -> replicate (m * n + 1) x) )