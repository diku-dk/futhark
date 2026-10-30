-- ==
-- input {
--   [-2.0f32,3.0f32,9.0f32]
-- }
-- output {
--   19.0f32
-- }
-- structure { Screma 1 }
def f (a: f32) : f32 = a + 3.0
def g (a: f32) : f32 = a * 3.0
def h (x: f32, y: (f32, f32)) : f32 = let (a, b) = y in a * b - (a + b) + x
def opp (x: f32) (a: f32) (b: f32) : f32 = x * (a + b)

def main (arr: []f32) : f32 =
  let arr2 = replicate 5 arr
  let y =
    map (\(x: []f32) : f32 ->
           let a = map f x
           let b = reduce (opp (1.0)) (0.0) a
           in b)
        arr2
  in y[0]
