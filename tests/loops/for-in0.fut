-- Basic for-in loop.
-- ==
-- input { [1,2,3,4,5] }
-- output { 15 }

def main (xs: []i32) =
  loop a = 0 for x in xs do a + x
