-- We can use operator sections anywhere, just like other functions.
-- ==
-- input { 5 3 } output { 2 2 -2 }

let (-^) (x: i32) (y: i32) = x - y
let main (x: i32) (y: i32) =
  ( (-^) x y
  , (x -^) y
  , (-^ x) y)
