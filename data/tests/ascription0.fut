-- Make sure type errors due to invalid type ascriptions are caught.
--
-- ==
-- error: .*cannot match.*

fun main(x: int, y:int): int =
  let ((a: int), b: int) : (bool,bool) = (x,y)
  in (a,b)
