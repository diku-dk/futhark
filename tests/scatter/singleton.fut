-- #1793
-- ==
-- input { [false] 0i64 } output { [true] }
-- input { [false] 1i64 } output { [false] }

def main (xs: *[1]bool) i =
  scatter xs [i] [true]
