-- ==
-- entry: rev 
-- compiled input { [1f32, 2f32, 3f32, 4f32] 3.0f32 } output { [72f32, 36f32, 24f32, 18f32] 24f32 }

def red_mult [n] (xs: [n]f32, c: f32) : f32 =
  reduce (*) 1 xs * c

entry rev [n] (xs: [n]f32) (c: f32) =
  vjp red_mult (xs,c) 1