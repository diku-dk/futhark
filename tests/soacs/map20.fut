-- The interesting thing here is that the compiler should simplify
-- away the copy.
-- ==
-- input { [[1,2,3],[4,5,6]] }
-- output { [[0.0f32, 2.0f32, 3.0f32], [0.0f32, 5.0f32, 6.0f32]] }
-- structure { Replicate 0 }

def main (xss: *[][]i32) =
  map (\xs -> map f32.i32 (copy xs with [0] = 0)) xss
