-- A simple test for index-function anti-unification across an if-then-else
-- ==
-- input  { [-1.0f32, 3.0f32, 5.0f32, 7.0f32, 9.0f32, 11.0f32, 13.0f32, 15.0f32, 17.0f32, 19.0f32, 21.0f32, 23.0f32, 25.0f32]}
-- output { [21.0f32, 23.0f32, 25.0f32] }
--
-- input  { [ 1.0f32, 3.0f32, 5.0f32, 7.0f32, 9.0f32, 11.0f32, 13.0f32, 15.0f32, 17.0f32, 19.0f32, 21.0f32, 23.0f32, 25.0f32]}
-- output { [ 2.0f32, 6.0f32, 10.0f32] }

def main [n] (arr: [n]f32) =
  let x =
    if (arr[0] < 0.0)
    then arr[10:n]
    else map (* 2.0f32) arr[0:n - 10]
  in x
