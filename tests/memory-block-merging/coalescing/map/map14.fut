-- This is testing that duplicate names for memory blocks in different
-- functions don't cause trouble.
-- ==
-- entry: foo bar
-- input { [1,2,3] [2,3,4] }
-- output { [3,5,7] }
-- structure gpu-mem { Alloc 0 }
-- structure mc-mem { Alloc 0 }
-- structure seq-mem { Alloc 2 }

entry foo (xs: *[]i32) (ys: []i32) =
  map2 (+) xs ys

entry bar (xs: *[]i32) (ys: []i32) =
  map2 (+) xs ys
