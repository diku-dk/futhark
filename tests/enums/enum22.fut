-- Non-exhaustive pattern match 3.
-- ==
-- error:

type planet = #mercury | #venus | #earth | #mars

def f (x: planet) : i32 =
  1
  + match x
    case #mercury -> 1
    case #venus -> 2
    case #earth -> 3

def main : i32 = f #mars
