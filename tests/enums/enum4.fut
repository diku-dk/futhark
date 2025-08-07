-- Test matches on nested tuples 4.
-- ==
-- input { }
-- output { 3 }

def main : i32 =
  match (4, (5, 6))
  case (_, (_, 10)) -> 1
  case (_, (_, 7)) -> 2
  case (_, (_, 6)) -> 3
  case _ -> 4
