-- The scrutinee of a 'match' expression is fully evaluated before the branches.
-- ==

def main (xs: *[]i32) =
  match xs[0]
  case 0 -> xs with [0] = 0
  case _ -> [0]
