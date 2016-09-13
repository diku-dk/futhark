-- https://rosettacode.org/wiki/Even_or_odd
--
-- True if even.
-- ==
-- input { 0 } output { True }
-- input { 1 } output { False }
-- input { 10 } output { True }
-- input { 11 } output { False }

fun main(x: int): bool = (x & 1) == 0
