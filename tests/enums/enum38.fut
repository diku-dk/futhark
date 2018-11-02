-- Missing pattern warning 8 (bool).
-- ==
-- error:

let f : bool =
  match (true, false)
    case (false, true) -> false
