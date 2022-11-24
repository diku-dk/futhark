-- ==
-- input { [1,2,3] [2,3,4] }
-- output { [3,5,7] }
-- structure gpu-mem { Alloc 0 }
-- structure mc-mem { Alloc 0 }
-- structure seq-mem { Alloc 1 }

def main (xs: *[]i32) (ys: []i32) =
  map2 (+) xs ys
