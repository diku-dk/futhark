-- Do records work at all?
-- ==
-- input { 2 } output { 1 3 }

def f (x: i32) = {y = x + 1, x = x - 1}

def main (x: i32) =
  let r = f x
  in (r.x, r.y)
