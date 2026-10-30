-- http://rosettacode.org/wiki/Averages/Pythagorean_means
--
-- ==
-- input { [1.0f32,2.0f32,3.0f32,1.0f32] }
-- output { 1.75f32 1.565f32 1.412f32 }

-- Divide first to improve numerical behaviour.
def arithmetic_mean [n] (as: [n]f32) : f32 =
  reduce (+) 0.0 (map (/ f32.i64 (n)) as)

def geometric_mean [n] (as: [n]f32) : f32 =
  reduce (*) 1.0 (map (** (1.0 / f32.i64 (n))) as)

def harmonic_mean [n] (as: [n]f32) : f32 =
  f32.i64 (n) / reduce (+) 0.0 (map (1.0 /) as)

def main (as: []f32) : (f32, f32, f32) =
  ( arithmetic_mean as
  , geometric_mean as
  , harmonic_mean as
  )
