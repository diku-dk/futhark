-- Left-side operands should be evaluated before before right-hand
-- operands.
-- ==
-- input { 2i64 } output { [[2i64,2i64,2i64]] }

def f (x: i64) : [][]i64 =
  [replicate (x + 1) 0]

def main x =
  f x |> map (map (+ 2))
