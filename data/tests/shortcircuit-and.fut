-- && must be short-circuiting.
--
-- ==
-- input { 0 [True, True] } output { True }
-- input { 1 [True, True] } output { True }
-- input { 2 [True, True] } output { False }

fun main(i: int, bs: [n]bool): bool =
  i < n && bs[i]
