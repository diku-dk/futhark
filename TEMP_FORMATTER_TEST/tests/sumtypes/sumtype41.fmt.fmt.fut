-- ==
-- error: Unmatched
type tuple = #tuple boolbool

def f (x: tuple) =
  match x
    case #tuple false false -> 0
    case #tuple true true -> 0