-- Matching type error 2.
-- ==
-- error:

type planet = #mercury | #venus | #earth | #mars

def x =
  match 2
  case #mercury -> 3
  case #venus -> 1
  case #earth -> true
  case #mars -> false
