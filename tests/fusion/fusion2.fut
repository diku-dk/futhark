-- ==
-- input {
--   [1.0,2.0,3.0,4.0]
-- }
-- output {
--   73.000000
-- }
def f (a: f64) : f64 = a + 3.0
def g (a: f64) : f64 = a * 3.0
def h (x: f64) (y: (f64, f64)) : f64 = let (a, b) = y in a * b - (a + b) + x

def main (arr: []f64) : f64 =
  let b = map f arr
  let x = map f b
  let y = map g b
  let z = map (h (x[1])) (zip x y)
  in z[0]

--+ y[0]
