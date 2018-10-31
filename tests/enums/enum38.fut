-- Missing pattern warning 8 (bool).
-- ==
-- warning: unmatched

let f : bool =
  match (true, false)
    case (false, true) -> false
