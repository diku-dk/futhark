-- We can close over shape parameters.
-- ==
-- input { [5,8,9] 5 } output { 8 }

let f [n] (_: [n]i32) =
  \(y:i32) -> y+n

let main (xs: []i32) (x: i32) = f xs x
