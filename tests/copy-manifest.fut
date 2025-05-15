-- Related to the copyManifest simplification rule.
-- ==
-- input { [[0,1,2],[4,5,6]] }
-- output { [0, 4, 1, 5, 2, 6] }
-- structure seq-mem { Alloc 1 Replicate 0 }
-- structure mc-mem { Alloc 1 Replicate 0 }
-- structure gpu-mem { Alloc 1 Replicate 0 }

entry main (xs: [][]i32) : *[]i32 =
  copy (flatten (transpose xs))
