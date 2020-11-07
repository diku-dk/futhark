-- ==
-- error: Unmatched

let f (x: (bool, bool)) =
  match x
  case (false, false) -> 0
  case (true, true) -> 0
