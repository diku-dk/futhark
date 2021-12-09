-- ==
-- error: Unmatched

def f (x: (i32, i32)) =
  match x
  case (0, _) -> 0
  case (_, 1) -> 0
