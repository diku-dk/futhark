-- ==
-- input { [1.0f32, 2.0f32, -4.0f32, 1.5f32] }
-- output { [13.0f32, 22.0f32, -2.0f32, 17.25f32] }

def f (a: f32) : f32 = a + 3.0
def g (a: f32) : f32 = a * 3.0
def h1 (a1: f32, a2: f32, a3: f32) : f32 = a1 * a2 + a3

--let f32 h2((f32,f32) a23) = let (a2,a3) = a23 in a2 * a3
def h2 (a1: f32) (a23: (f32, f32)) : f32 = let (a2, a3) = a23 in a2 * a3 - a1

def main (arr: []f32) : []f32 =
  let x = map f arr
  let y = map g arr
  in if arr[0] < 0.0
     then map h1 (zip3 x y x)
     else --else map(h2(1.0), zip(y,x))
          map (h2 (y[0])) (zip x x)
