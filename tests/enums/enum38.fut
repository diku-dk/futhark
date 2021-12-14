-- Missing pattern warning 8 (bool).
-- ==
-- error:

def f : bool =
  match (true, false)
    case (false, true) -> false
