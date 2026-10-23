-- http://rosettacode.org/wiki/Averages/Root_mean_square
--
-- input { [1.0,2.0,3.0,1.0] }
-- output { 1.936f32 }

def main [n] (as: [n]f32) : f32 =
  f32.sqrt ((reduce (+) 0.0 (map (** 2.0) as)) / f32.i64 n)
