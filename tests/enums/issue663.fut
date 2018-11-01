-- Issue 663. x shouldn't need a type ascription.
-- ==

let main: (bool, #l | #r) =
  let x = #l
  in (x == x, x)
