-- This test shows how to include a file and use its function.
-- ==
-- input {
--   7
-- }
-- output {
--   29
-- }

include include_basic_includee

fun main(s: i32): i32 = includee_function(s) + 1
