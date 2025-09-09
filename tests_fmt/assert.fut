-- Try to format asserts in a way that is not awful.

def norm [n] (xs: [n]f32) =
  assert (n > 0)
  (let m = f32.maximum
   in map (/ m) xs)
