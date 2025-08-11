-- Implicit field expressions.
-- ==
-- input { 1 2 } output { 1 2 }

def main (x: i32) (y: i32) =
  let r = {y, x}
  in (r.x, r.y)
