-- If a size is produced by similar arguments in different places in
-- the program, those should be considered distint.
-- ==
-- input { true [1,2,3] } output { [0i64,1i64,2i64] }
-- input { false [1,2,3] } output { [0i64,1i64,2i64] }

def main (b: bool) (xs: []i32) =
  if b
  then let arr = iota (length xs) in arr
  else let arr = iota (length xs) in arr
