-- We can close over shape parameters.
-- ==
-- input { [5,8,9] 5i64 } output { 8i64 }

def f [n] (_: [n]i32) =
  \(y: i64) -> y + n

def main (xs: []i32) (x: i64) = f xs x
