-- ==
-- entry: concat_iotas
-- input { 2i64 4i64 }
-- output { [ 0i64, 1i64, 0i64, 1i64, 2i64, 3i64 ] }
-- structure gpu-mem { Alloc 1 }
-- structure seq-mem { Alloc 1 }

entry concat_iotas (i: i64) (j: i64) : []i64 =
  concat (iota i) (iota j)
