-- ==
-- structure { Screma 3 }

def f (a: f64) : f64 = a + 3.0
def g (a: []f64) (b: f64) : f64 = a[0] * b
def h (a: f64) (b: f64) : f64 = a * b

def main (arr: []f64) : f64 =
  let x = map f arr
  let y = map (g (x)) x
  let z = map (h (y[0])) y
  in z[0]
