-- ==
-- input { empty([0][2]i32) }
-- output { empty([0]i32) }

def main (xs: [][]i32) =
  (transpose xs)[0]
