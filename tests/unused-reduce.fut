-- Can we remove the unused parts of a reduction?
-- ==
-- structure { Screma/BinOp 1 }

def main (xs: []i32) (ys: []i32) =
  let (x', _) =
    reduce (\(x1, y1) (x2, y2) -> (x1 + x2, y1 * y2))
           (0, 1)
           (zip xs ys)
  in x'
