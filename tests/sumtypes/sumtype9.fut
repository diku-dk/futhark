-- Sumtype matches on wildcards.
-- ==
-- input { }
-- output { 2 }

type foobar = #foo i32 | #bar i16

def main : i32 =
  match ((#bar 1) : foobar)
  case (#foo _) -> 1
  case (#bar _) -> 2
