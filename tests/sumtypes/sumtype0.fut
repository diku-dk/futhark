-- Basic sum type.
-- ==
-- input { } 
-- output { 5 }

type foobar = #foo i32 | #bar i16

let main : i32 =
  match (#foo 5) : foobar
  case (#bar 5) -> 1
  case (#foo 4) -> 2
  case (#foo x) -> x
