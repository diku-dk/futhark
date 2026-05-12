-- Missing pattern warning 5 (integers).
-- ==
-- error: Unmatched

def f : i32 =
  match (1 : i32)
  case 1 -> 1
  case 2 -> 2
