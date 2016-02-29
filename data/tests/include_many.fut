-- This test shows how to include many file and use their functions.
-- ==
-- input {
--   2
-- }
-- output {
--   -5
-- }

include include_many_includee0
include include_many_includee1

fun i32 main(i32 s) =
  includee0_function(s)
  + includee1_function(s) * includee0_includee_function(s)
