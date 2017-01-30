-- Fail if the argument to write is not unique.
--
-- ==
-- error: .*unique.*

fun main(a: []i32): []i32 =
  write ([0]) ([1]) a
