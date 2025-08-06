-- Non-exhaustive pattern match 2.
-- ==
-- error:

type planet = #mercury | #venus | #earth | #mars

def main : i32 =
  match (1, #mars : planet, 5)
  case (1, #mercury, 5) -> 1
  case (1, #venus, 5) -> 2
  case (1, #earth, 5) -> 3
  case (1, #earth, 6) -> 4
