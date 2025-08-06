-- Small copy test.
-- ==
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 1 }

def main (length: i64) : [length]i32 =
  let temp = replicate length 1i32
  -- Will be moved up to before temp.
  let with_hoistable_mem = copy temp
  in with_hoistable_mem
