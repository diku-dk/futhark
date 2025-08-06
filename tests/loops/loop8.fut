-- Test that we do not clobber loop merge parameters while they may
-- still be used.
-- ==
-- input { 1 2 0 3 }
-- output { 2 1 3 }

def main (x: i32) (y: i32) (z: i32) (n: i32) : (i32, i32, i32) =
  loop ((x, y, z)) for i < n do (y, x, z + 1)
