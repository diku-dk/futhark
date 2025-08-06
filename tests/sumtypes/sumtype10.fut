-- Inexhaustive sumtype pattern match.
-- ==
-- error: Unmatched cases

type foobar = #foo i32 | #bar i32

def main : i32 =
  match ((#bar 12) : foobar)
  case (#foo _) -> 1
