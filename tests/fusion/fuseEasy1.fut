-- ==
-- input {
--   [1.0f32,-4.0f32,-2.4f32]
-- }
-- output {
--   36.000000f32
-- }
def f (a: f32) : f32 = a + 3.0
def g (a: f32) : f32 = a * 3.0

def main (arr: []f32) : f32 =
  let x = map f arr
  let y = map g x
  let z = map g y
  in z[0]
