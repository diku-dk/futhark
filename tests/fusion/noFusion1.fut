-- ==
def f (a: f32) : f32 = a + 3.0
def g (a: f32) (b: f32) : f32 = a * b

def main (arr: []f32) : f32 =
  let n = t32 (arr[0])
  let x = map f arr
  let arr =
    loop (arr) for i < n do
      map (g (arr[i])) x
  in arr[0]
