def record (x: (f32, f32)) =
  x with 0 = 42
       with 1 = 1337

def array (x: *[]f32) =
  x with [0] = 42 with
  [1] = 1337
