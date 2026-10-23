-- http://rosettacode.org/wiki/Averages/Arithmetic_mean
--
-- ==
-- input { [1.0f32,2.0f32,3.0f32,1.0f32] }
-- output { 1.75f32 }

-- Divide first to improve numerical behaviour.
def main [n] (as: [n]f32) : f32 =
  reduce (+) 0f32 (map (/ f32.i64 (n)) as)
