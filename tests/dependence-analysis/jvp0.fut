-- ==
-- structure { BinOp 2 }

def main (A: [](i32, i32)) (n: i64) =
  let r =
    loop A for _i < n do
      jvp (map (\(a, b) -> (a * a, b * b))) A A
  in map (.0) r
