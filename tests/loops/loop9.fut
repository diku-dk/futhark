-- Test that we can remove a single-iteration loop.
-- ==
-- structure { Loop 0 }

def main(x: i32, y: i32): i32 =
  loop (x) for i < 1 do x + y
