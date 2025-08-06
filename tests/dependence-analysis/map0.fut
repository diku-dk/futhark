-- ==
-- structure { BinOp 1 }

def main (A: [](i32, i32)) (n: i64) =
  let r =
    loop A for i < n do
      map (\(a, b) -> if i == 0 then (a, b) else (a + 1, b + 1)) A
  in map (.0) r
