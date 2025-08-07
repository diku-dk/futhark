-- Optimally, we would test just the the expresions are hoisted as we expect,
-- but currently we just test that the final number of allocations match (it is
-- the same for the other hoisting tests).
-- ==
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 1 }

def main (length0: i64, length1: i64) : []i32 =
  let temp0 = replicate length0 1i32
  let temp1 = replicate length1 1i32
  -- Will be moved up to before temp0.
  let with_hoistable_mem = concat temp0 temp1
  in with_hoistable_mem
