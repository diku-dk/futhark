-- ==
def f (a: f32) : f32 = a + 3.0
def g (a: f32) : f32 = a * 3.0

def main (arr: []f32) : f32 =
  let x = map f arr
  let y = map f x
  let z = map g x
  in y[0] + z[0]
