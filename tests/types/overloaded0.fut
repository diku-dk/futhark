-- Test of overloaded numeric types.
-- ==
-- input { 10f32 } output { 0.01f32 }

def main (x: f32) =
  let y = 0.001
  let f (z: f32) = y * z
  in f x
