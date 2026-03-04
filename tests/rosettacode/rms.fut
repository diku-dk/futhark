-- http://rosettacode.org/wiki/Averages/Root_mean_square
-- ==
-- tags { no_webgpu }
-- input { [1.0,2.0,3.0,1.0] }
-- output { 1.936f64 }

def main [n] (as: [n]f64) : f64 =
  f64.sqrt ((reduce (+) 0.0 (map (** 2.0) as)) / f64.i64 n)
