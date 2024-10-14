-- ==
-- entry: rev
-- input { [1f32, 0f32, 3f32, 4f32] 3.0f32 } output { [0f32, 36f32, 0f32, 0f32] 0f32 }

def red_mult [n] (xs: [n]f32, c: f32) : f32 =
  reduce (*) 1 xs * c

entry rev [n] (xs: [n]f32) (c: f32) =
  vjp red_mult (xs,c) 1