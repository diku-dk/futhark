-- Make sure type errors due to invalid type ascriptions are caught.
--
-- ==
-- error: match

def main (x: i32, y: i32) : i32 =
  let (((a): i32), b: i32): (bool, bool) = (x, y)
  in (a, b)
