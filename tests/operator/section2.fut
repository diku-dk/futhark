-- Backticked operator sections work.
-- ==
-- input { 5 3 } output { 2 2 -2 }

let minus (x: i32) (y: i32) = x - y
let main (x: i32) (y: i32) =
  ( (`minus`) x y
  , (x `minus`) y
  , (`minus` x) y)
