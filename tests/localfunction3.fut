-- Local functions can be shadowed.
-- ==
-- input { 3 } output { 10 }

def main (x: i32) =
  let f (y: i32) = y + 2
  let x = f x
  let f (y: i32) = y * 2
  let x = f x
  in x
