-- http://rosettacode.org/wiki/Averages/Arithmetic_mean
--
-- ==
-- input { [1.0,2.0,3.0,1.0] }
-- output { 1.75f64 }

-- Divide first to improve numerical behaviour.
def main [n] (as: [n]f64) : f64 =
  reduce (+) 0f64 (map (/ f64.i64 (n)) as)
