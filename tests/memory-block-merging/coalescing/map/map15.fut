-- ==
-- input { [1,2,3] }
-- output { [2.0f32, 3.0f32, 4.0f32] }
-- structure gpu-mem { Alloc 0 }

def main [n] (xs: *[n]i32) : *[n]f32 =
  let xs = opaque (map (+ 1) xs)
  let ys = map (f32.i32) xs
  in ys
