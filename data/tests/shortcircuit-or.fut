-- && must be short-circuiting.
--
-- ==
-- input { 0 [False, False] } output { False }
-- input { 1 [False, False] } output { False }
-- input { 2 [False, False] } output { True }

fun main(i: int, bs: [n]bool): bool =
  i >= n || bs[i]
