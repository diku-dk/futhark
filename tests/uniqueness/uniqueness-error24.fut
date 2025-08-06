-- Test that consumption checking is done even with no meaningful
-- bindings.
-- ==
-- error: consumed

def consume (a: *[]i32) : i32 = 0

-- OK.

def main (a: *[]i32) : []i32 =
  let _ = consume (a)
  in a

-- Should fail, because a has been consumed!
