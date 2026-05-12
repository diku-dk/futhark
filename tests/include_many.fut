-- This test shows how to include many file and use their functions.
-- ==
-- input {
--   2
-- }
-- output {
--   -5
-- }

import "include_many_includee0"
import "include_many_includee1"

def main (s: i32) : i32 =
  includee0_function (s)
  + includee1_function (s) * includee0_includee_function (s)
