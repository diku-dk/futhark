-- Tuples can be used like records.
-- ==
-- input { 2 } output { 3 1 }

def f (x: i32) = (x + 1, x - 1)

def main (x: i32) =
  let r = f x
  in (r.0, r.1)
