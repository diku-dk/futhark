-- Non-exhaustive pattern match 3.
-- ==
-- input { }
-- error:

type planet = #mercury | #venus | #earth | #mars

let f (x : planet) : i32 = 
 1 + match x
       case #mercury -> 1
       case #venus   -> 2
       case #earth   -> 3

let main : i32 = f #mars
