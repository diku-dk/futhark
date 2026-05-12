-- A small chain of coalescings.
-- ==
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 1 }

def main (length: i64) : []i32 =
  let temp = replicate length 1i32
  -- Will be moved up to before temp.
  let with_hoistable_mem0 = copy temp
  -- Will be moved up to before temp.
  let with_hoistable_mem1 = concat temp with_hoistable_mem0
  in with_hoistable_mem1
