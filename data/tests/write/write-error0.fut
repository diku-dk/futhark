-- Fail if the argument to write is not unique.
--
-- ==
-- error: .*unique.*

fun main(a: []int): []int =
  write ([0]) ([1]) (a)
