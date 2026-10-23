-- ==
-- input { [4.0f32, 2.0f32, -4.0f32, 1.5f32] }
-- output { 17.0f32 }

def f (a: f32, b: f32) : f32 = a + 3.0
def g (a: f32, b: f32) : f32 = a * 3.0

def main (arr: []f32) : f32 =
  let n = i64.f32 arr[0]
  let x = replicate n 2.0
  let y = map f (zip x (arr :> [n]f32))
  let z = map g (zip (arr :> [n]f32) x)
  in y[0] + z[0]
