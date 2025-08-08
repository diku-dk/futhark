-- ==
def f (a: f64) : f64 = a + 3.0
def g (a: f64) (b: f64) : f64 = a * b

def main (arr: []f64) : f64 =
  let n = t64 (arr[0])
  let x = map f arr
  let arr =
    loop (arr) for i < n do
      map (g (arr[i])) x
  in arr[0]
