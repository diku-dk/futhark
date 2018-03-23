-- Test that the filter function must take nonunique arguments.
-- Disabled until #501 is resolved.
-- ==
-- tags { disable }
-- error:
let main(a: *[][]i32): [][]i32 =
  let _ = filter (\(r: *[]i32): bool  -> true) a
  in empty([]i32)
