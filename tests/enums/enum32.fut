-- Missing pattern warning 2.
-- ==
-- error: Unmatched

type planet = #mercury | #venus | #earth | #mars
type foobar = #foo | #bar
type rec = {f1: foobar, f2: planet}

def g : i32 =
  match {f1 = #bar, f2 = #earth} : rec
  case {f1 = #bar, f2 = #venus} -> 1
