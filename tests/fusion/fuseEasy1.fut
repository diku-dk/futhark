-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   36.000000
-- }
def f (a: f64) : f64 = a + 3.0
def g (a: f64) : f64 = a * 3.0

def main (arr: []f64) : f64 =
  let x = map f arr
  let y = map g x
  let z = map g y
  in z[0]
