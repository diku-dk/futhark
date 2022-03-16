-- ==
-- error: Unexpected end of file

def main (x:i32): f32 =
  let f_ = map f (1..<x)
  let k_ = kura f_ 1f32 0.1f32

  reduce (+) 0.0f32 k_
