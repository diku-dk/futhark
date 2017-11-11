-- http://rosettacode.org/wiki/Averages/Pythagorean_means
--
-- ==
-- input { [1.0,2.0,3.0,1.0] }
-- output { 1.75f64 1.565f64 1.412f64 }

-- Divide first to improve numerical behaviour.
let arithmetic_mean [n] (as: [n]f64): f64 =
  reduce (+) 0.0 (map (/f64(n)) as)

let geometric_mean [n] (as: [n]f64): f64 =
  reduce (*) 1.0 (map (**(1.0/f64(n))) as)

let harmonic_mean [n] (as: [n]f64): f64 =
  f64(n) / reduce (+) 0.0 (map (1.0/) as)

let main(as: []f64): (f64,f64,f64) =
  (arithmetic_mean as,
   geometric_mean as,
   harmonic_mean as)