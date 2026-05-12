-- A loop that doesn't involve in-place updates or even arrays.
-- ==
-- input {
--   42
-- }
-- output {
--   861
-- }

def main (n: i32) : i32 =
  loop x = 0 for i < n do x + i
