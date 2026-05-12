-- Test that we can curry even "complex" arguments.
-- ==
-- input {
--   [1.0,6.0,3.0,4.0,1.0,0.0]
-- }
-- output {
--   252.000000
-- }

def f (x: (i64, f64)) (y: f64) : f64 =
  let (a, b) = x in y * f64.i64 (a) + b

def g (x: [](f64, f64)) (y: f64) : f64 =
  let (a, b) = unzip (x)
  in y + reduce (+) (0.0) a + reduce (+) (0.0) b

def main (a: []f64) : f64 =
  let b = map (f ((5, 6.0))) a
  let c = map (g (zip ([1.0, 2.0, 3.0]) ([4.0, 5.0, 6.0]))) a
  in reduce (+) (0.0) b + reduce (+) (0.0) c
