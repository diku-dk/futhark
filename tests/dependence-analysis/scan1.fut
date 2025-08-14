-- A simple streamSeq; does not exercise the Stream case in opDependencies,
-- but dead code is still removed by the Screma case.
-- ==
-- structure { Stream/BinOp 2 }
-- structure { Screma/BinOp 2 }

def plus (a, b) (x, y) : (i32, i32) = (a + x, b + y)

def main (xs: [](i32, i32)) (n: i64) =
  let r =
    loop xs for i < n do
      map (\(x, y) -> if i == 0 then (x, y) else (x + 1, y + 2)) (scan plus (0, 0) xs)
  in map (.0) r
