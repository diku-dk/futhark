-- Test that the filter function must take nonunique arguments.
-- ==
-- error:
fun [[int]] main(*[[int]] a) =
  let _ = filter(fn bool (*[int] r) => True, a)
  in empty([int])
