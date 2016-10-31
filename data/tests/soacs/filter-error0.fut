-- Test that the filter function must take nonunique arguments.
-- ==
-- error:
fun main(a: *[][]int): [][]int =
  let _ = filter (fn (r: *[]int): bool  => true) a
  in empty([]int)
