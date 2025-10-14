-- Appling a first-order function that returns a lambda which closes
-- over a local variable.
-- ==
-- input { 3 4 } output { 9 }
-- input { 10 12 } output { 24 }

def main (x: i32) (y: i32) =
  let f (x: i32) =
    let a = 2
    in \(y: i32) -> x + y + a
  in f x y
