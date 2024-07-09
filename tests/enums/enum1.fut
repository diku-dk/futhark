-- Matches on nested tuples 1.
-- ==
-- input { }
-- output { 3 }

def main : i32 =
  match ((3,(1,10)), 2)
    case (_, 3)           -> 1
    case ((4,_), 2)       -> 2
    case ((3,(_, 10)), _) -> 3
    case (_, _)           -> 4
