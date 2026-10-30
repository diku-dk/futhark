-- Test that we can curry even "complex" arguments.
-- ==
-- input {
--   [1.0f32,6.0f32,3.0f32,4.0f32,1.0f32,0.0f32]
-- }
-- output {
--   252.000000f32
-- }

def f (x: (i64, f32)) (y: f32) : f32 =
  let (a, b) = x in y * f32.i64 (a) + b

def g (x: [](f32, f32)) (y: f32) : f32 =
  let (a, b) = unzip (x)
  in y + reduce (+) (0.0) a + reduce (+) (0.0) b

def main (a: []f32) : f32 =
  let b = map (f ((5, 6.0))) a
  let c = map (g (zip ([1.0, 2.0, 3.0]) ([4.0, 5.0, 6.0]))) a
  in reduce (+) (0.0) b + reduce (+) (0.0) c
