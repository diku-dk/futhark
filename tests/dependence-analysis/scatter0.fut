-- ==
-- structure { Replicate 1 }

def main [n] (A: *[n](i32,i32)) =
  let r =
    loop A for _i < n do
      scatter A (iota n) (copy A)
  in map (.0) r
