-- Missing pattern warning 2.
-- ==
-- warning: unmatched

type planet = #mercury | #venus | #earth | #mars
type foobar = #foo | #bar
type rec    = {f1 : foobar, f2: planet}

let g : i32 =
  match {f1 = #bar, f2 = #earth} : rec
    case {f1 = #bar, f2 = #venus} -> 1
