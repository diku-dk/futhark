-- Make sure ascribed names are available.
--
-- ==
-- input { 2 [1,2,3] }
-- output { 4 }

fun main ((_, elems: []i32): (i32,[n]i32)): i32 =
  n + elems[0]
