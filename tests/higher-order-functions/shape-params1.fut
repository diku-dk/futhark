-- We can close over shape parameters.
-- ==
-- input { [5,8,9] 5i64 } output { 8i64 }

let f [n] (_: [n]i32) =
  \(y:i64) -> y+n

let main (xs: []i32) (x: i64) = f xs x
