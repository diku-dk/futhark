-- Multidimensional array literals should become one array literal in
-- the core language.
-- ==
-- input { 2 } output { [[1,2],[2,2]] }
-- structure { ArrayLit 1 }

def main (x: i32) : [][]i32 =
  [[1, x], [2, x]]
