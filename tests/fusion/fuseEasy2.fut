-- ==
-- input {
--   [1.0f32,-4.0f32,-2.4f32]
-- }
-- output {
--   16.000000f32
-- }
def f (a: f32) : f32 = a + 3.0
def g (a: f32) : f32 = a * 3.0
def h (a1: f32, a2: f32, a3: f32) : f32 = a1 * a2 + a3

def main (arr: []f32) : f32 =
  let x = map f arr
  let y = map g arr
  let z = map h (zip3 x y x)
  in z[0]
