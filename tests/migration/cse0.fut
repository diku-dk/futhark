-- Duplicate scalar migrations are eliminated.
-- ==
-- structure gpu {
--   Replicate 1
-- }

def main (A: [5]i32) (x: i32) : i32 =
  let (a, b) =
    if x == 42
    then (0, 0)
    else (A[0], A[1])
  in a + b
