-- ==
-- input {
--   [-2.0,3.0,9.0]
-- }
-- output {
--   19.0
-- }
-- structure { Screma 1 }
def f (a: f64) : f64 = a + 3.0
def g (a: f64) : f64 = a * 3.0
def h (x: f64, y: (f64, f64)) : f64 = let (a, b) = y in a * b - (a + b) + x
def opp (x: f64) (a: f64) (b: f64) : f64 = x * (a + b)

def main (arr: []f64) : f64 =
  let arr2 = replicate 5 arr
  let y =
    map (\(x: []f64) : f64 ->
           let a = map f x
           let b = reduce (opp (1.0)) (0.0) a
           in b)
        arr2
  in y[0]
