-- Tragic problem with index functions.
-- ==
-- input { true 1i64 2i64 [1,2,3] } output { [1,2] }
-- input { false 1i64 2i64 [1,2,3] } output { [1] }

def main (b: bool) (n: i64) (m: i64) (xs: []i32) =
  if b then xs[0:m] else xs[0:n]
