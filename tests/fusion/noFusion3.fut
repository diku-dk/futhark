-- ==
-- structure { Screma 3 }

def f (a: f32) : f32 = a + 3.0
def g (a: []f32) (b: f32) : f32 = a[0] * b
def h (a: f32) (b: f32) : f32 = a * b

def main (arr: []f32) : f32 =
  let x = map f arr
  let y = map (g (x)) x
  let z = map (h (y[0])) y
  in z[0]
