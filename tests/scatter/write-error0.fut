-- Fail if the argument to write is not unique.
--
-- ==
-- error: .*unique.*

let main(a: []i32): []i32 =
  scatter a [0] [1]
