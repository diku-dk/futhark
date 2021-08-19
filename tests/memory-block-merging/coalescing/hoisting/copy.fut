-- Small copy test.
-- ==
-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 1 }

let main (length: i64): [length]i32 =
  let temp = replicate length 1i32

  -- Will be moved up to before temp.
  let with_hoistable_mem = copy temp
  in with_hoistable_mem
