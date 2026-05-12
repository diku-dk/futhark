-- ==
-- input {
--   [1.0,2.0,3.0,4.0]
-- }
-- output {
--   65.000000
-- }
def f (a: f64) : f64 = a + 3.0
def g (a: f64) : f64 = a * 3.0
def h (a: f64, b: f64) : f64 = a * b - (a + b)

def main (arr: []f64) : f64 =
  let b = map f arr
  --let arr[1] = 3.33   in
  let x = map f b
  let y = map g b
  let z = map h (zip x y)
  in z[0]
