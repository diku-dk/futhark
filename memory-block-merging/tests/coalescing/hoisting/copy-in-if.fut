-- In this case the nested body needs allocation hoisting to enable one array
-- coalescing.
--
-- It is perhaps a pretty far-out case.
-- ==
-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 2 }

import "/futlib/array"

let main (cond: bool, lengths: []i32, index: i32): []i32 =
  if cond
  then let lengths' = map (+1) lengths
       let temp = replicate lengths'[index] 1i32

       -- Will be moved up to before temp.
       let with_hoistable_mem = copy temp
       in with_hoistable_mem
  else lengths
