-- http://rosettacode.org/wiki/Averages/Pythagorean_means
--
-- ==
-- input { [1.0,2.0,3.0,1.0] }
-- output { 1.75f64 1.565f64 1.412f64 }

-- Divide first to improve numerical behaviour.
def arithmetic_mean [n] (as: [n]f64) : f64 =
  reduce (+) 0.0 (map (/ f64.i64 (n)) as)

def geometric_mean [n] (as: [n]f64) : f64 =
  reduce (*) 1.0 (map (** (1.0 / f64.i64 (n))) as)

def harmonic_mean [n] (as: [n]f64) : f64 =
  f64.i64 (n) / reduce (+) 0.0 (map (1.0 /) as)

def main (as: []f64) : (f64, f64, f64) =
  ( arithmetic_mean as
  , geometric_mean as
  , harmonic_mean as
  )
