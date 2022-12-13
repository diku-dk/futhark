-- #1796
-- input { [1,2] }
-- output { [[1, 2], [1, 2], [1, 2], [1, 2], [1, 2]] }
-- structure { Replicate 1 Concat 0 }

def main (xs: []i32) =
  replicate 2 xs ++ replicate 3 xs
