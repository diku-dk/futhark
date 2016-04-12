-- Fail if the argument to write is not unique.
--
-- ==
-- error: .*unique.*

fun [int] main([int] a) =
  write([0], [1], a)
