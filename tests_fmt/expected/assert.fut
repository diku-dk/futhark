-- Try to format asserts in a way that is not awful.

def norm [n] (xs: [n]f32) =
  assert (n > 0)
  assert (f32.maximum xs != 0)
  let m = f32.maximum xs
  in map (/ m) xs

def singleline x y = assert (y != 0) y
