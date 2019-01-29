-- Fail if lacking a type annotation.
-- ==
-- error:

let main : i32 =
  match (#bar 12)
  case (#foo _)  -> 1
  case (#bar _) -> 2