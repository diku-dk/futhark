-- Matching type error 1.
-- ==
-- error:

type planet = #mercury | #venus | #earth | #mars

def x = match (#mercury : planet)
          case #mercury -> 3
          case #venus   -> 1
          case #earth   -> true
          case #mars    -> false
