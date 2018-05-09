-- This used to produce an unnecessarily unique return type on a
-- lifted function.

let main =
  ((\x -> x) <-< (\x -> x)) [1,2,3]
