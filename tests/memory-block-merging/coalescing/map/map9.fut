-- ==
-- input { [1,2,3] }
-- output { [2,3,4] }
-- structure gpu-mem { Alloc 1 }
-- structure mc-mem { Alloc 1 }
-- structure seq-mem { Alloc 1 }

let main [n] (xs: [n]i32): [n]i32 =
  map (+1) xs
