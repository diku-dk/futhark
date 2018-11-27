-- Missing pattern warning 1.
-- ==
-- error:

type planet = #mercury | #venus | #earth | #mars

let g : i32 =
  match (#venus : planet)
    case #mercury -> 1
    case #venus   -> 2
    case #earth   -> 3
