-- This test shows how to include a file and use its function.
-- ==
-- input {
--   7
-- }
-- output {
--   29
-- }

include include_basic_includee.fut.module

fun i32 main(i32 s) = includee_function(s) + 1
