-- Assertion of functional value.
-- ==
-- input { 2 } output { 1 }
-- input { 3 } error: x % 2 == 0

def main (x: i32) =
  let f = assert (x % 2 == 0) (x /)
  in f 2
