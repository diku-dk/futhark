-- Non-exhaustive pattern match.
-- ==
-- input { }
-- error: 

type planet = #mercury | #venus | #earth | #mars

let main : i32 = match #mars : planet
                  case #mercury -> 1
                  case #venus   -> 2
                  case #earth   -> 3
