-- ==
-- error: Unmatched

type tuple = #tuple bool bool

let f (x: tuple) =
  match x
  case #tuple false false -> 0
  case #tuple true true -> 0
