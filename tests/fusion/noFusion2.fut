-- ==
def f (a: f64) : f64 = a + 3.0
def g (a: f64) : f64 = a * 3.0

def main (arr: []f64) : f64 =
  let x = map f arr
  let y = map f x
  let z = map g x
  in y[0] + z[0]
