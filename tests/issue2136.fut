def test (a: i64) (b: i64) (f: [a * b]f32 -> [a * b]f32) (g: [a * b]f32 -> [a * b]f32) =
  g (f (replicate (a * b) 0))

entry main a b =
  let h = test a b
  in h reverse reverse
