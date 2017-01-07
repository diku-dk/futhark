-- Using the same name twice in a single pattern is forbidden.
--
-- ==
-- error: Duplicate.*'y'.*

fun main (x: int): (int,int) =
  let (y,y) = (x-1, x+1)
  in (y,y)
