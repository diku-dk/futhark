-- Missing pattern warning 8 (bool).
-- ==
-- error: Unmatched

def f : bool =
  match (true, false)
  case (false, true) -> false
