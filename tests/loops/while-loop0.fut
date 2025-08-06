-- Simplest test of while loops.
-- ==
-- input {
--   1
--   9
-- }
-- output {
--   16
-- }

def main (x: i32) (bound: i32) : i32 =
  loop (x) while x < bound do x * 2
