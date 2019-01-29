-- Inexhaustive sumtype pattern match.
-- ==
-- error: 

type foobar = #foo i32 | #bar i32

let main : i32 =
  match ((#bar 12) : foobar)
  case (#foo _)  -> 1