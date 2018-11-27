let f (b: f32): (i32, []f32) =
  (0, [b])

let main (i: i32) =
  (\i -> f (f32.i32 i)) i
