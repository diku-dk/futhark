-- This test shows how to import a file and use its function.
-- ==
-- input {
--   7
-- }
-- output {
--   29
-- }

import "include_basic_includee"

def main (s: i32) : i32 = importe_function (s) + 1
