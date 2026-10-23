-- ==
-- input {
--   [1.0f32,2.0f32,3.0f32,4.0f32]
-- }
-- output {
--   65.000000f32
-- }
def f (a: f32) : f32 = a + 3.0
def g (a: f32) : f32 = a * 3.0
def h (a: f32, b: f32) : f32 = a * b - (a + b)

def main (arr: []f32) : f32 =
  let b = map f arr
  --let arr[1] = 3.33   in
  let x = map f b
  let y = map g b
  let z = map h (zip x y)
  in z[0]
