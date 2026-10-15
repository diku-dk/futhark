-- The arguments of an application are evaluated before the function, so
-- evaluating the function may consume what an argument used.
-- ==
-- input { [5, 6] } output { 6 }

def main (ys: *[]i32) =
  (let _ = ys with [0] = 1 in \(x: i32) -> x + 1) ys[0]
