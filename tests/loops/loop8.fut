-- Test that we do not clobber loop merge parameters while they may
-- still be used.
-- ==
-- input { 1 2 0 3 }
-- output { 2 1 3 }

fun main(x: int, y: int, z: int, n: int): (int, int, int) =
  loop ((x,y,z)) = for i < n do
    (y,x,z+1) in
  (x,y,z)
