-- Missing pattern warning 1.
-- ==
-- error: Unmatched

type planet = #mercury | #venus | #earth | #mars

def g : i32 =
  match (#venus : planet)
  case #mercury -> 1
  case #venus -> 2
  case #earth -> 3
