-- Matches on nested tuples 3.
-- ==
-- input { }
-- output { 3 }

type animal = #dog | #cat | #mouse | #bird

def main : i32 =
  match ((1, #dog : animal), 12, (#cat : animal, #mouse : animal))
    case ((6, #dog), 12, (#cat , #mouse)) -> 1
    case (_, 13, _)                       -> 2
    case ((1, #dog), 12, (#cat, #mouse))  -> 3
    case _                                -> 4
