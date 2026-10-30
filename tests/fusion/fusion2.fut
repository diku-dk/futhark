-- ==
-- input {
--   [1.0f32,2.0f32,3.0f32,4.0f32]
-- }
-- output {
--   73.000000f32
-- }
def f (a: f32) : f32 = a + 3.0
def g (a: f32) : f32 = a * 3.0
def h (x: f32) (y: (f32, f32)) : f32 = let (a, b) = y in a * b - (a + b) + x

def main (arr: []f32) : f32 =
  let b = map f arr
  let x = map f b
  let y = map g b
  let z = map (h (x[1])) (zip x y)
  in z[0]

--+ y[0]
