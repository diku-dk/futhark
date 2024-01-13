-- In this case the nested body needs allocation hoisting to enable one array
-- coalescing.
--
-- It is perhaps a pretty far-out case.
-- ==
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 2 }

let main (cond: bool, lengths: []i64, index: i64): []i64 =
  if cond
  then let lengths' = map (+1) lengths
       let temp = replicate lengths'[index] 1i64

       -- Will be moved up to before temp.
       let with_hoistable_mem = copy temp
       in with_hoistable_mem
  else lengths
