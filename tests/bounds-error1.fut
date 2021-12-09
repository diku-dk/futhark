-- The bounds error message should not refer to more dimensions than
-- are present in the source language program.
-- ==
-- input { [[1,2]] 4 }
-- error: Index \[4\] out of bounds for array of shape \[1\]

def index xs i = xs[i]

def main (xss: [][]i32) (i: i32) =
  index xss i
