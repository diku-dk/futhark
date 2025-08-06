-- Matches on nested tuples within sumtypes.
-- ==
-- input { }
-- output { 4 }

type foobar = #foo ((i32, (i32, i32)), i32) | #bar i32

def main : i32 =
  match (#foo ((3, (1, 10)), 2)) : foobar
  case (#foo (_, 3)) -> 1
  case (#foo ((4, _), 2)) -> 2
  case (#bar 5) -> 3
  case (#foo ((3, (_, 10)), _)) -> 4
  case (#bar _) -> 5
  case (#foo _) -> 6
