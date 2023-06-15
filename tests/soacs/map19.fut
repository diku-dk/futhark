-- The interesting thing here is that the compiler should simplify
-- away the copy.
-- ==
-- input { [[1,2,3],[4,5,6]] }
-- output { [[0, 2, 3], [0, 5, 6]] }
-- structure { Replicate 0 }

def main (xss: *[][]i32) =
  map (\xs -> copy xs with [0] = 0) xss
