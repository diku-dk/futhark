-- Issue 663. x shouldn't need a type ascription.
-- ==

def main : (bool, #l | #r) =
  let x = #l
  in (x == x, x)
