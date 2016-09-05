-- Using the same parameter name twice is forbidden.
--
-- ==
-- error: Duplicate.*x.*

fun main (x: int) (x: int): int = x
