-- An example of a program with silly limits.
-- ==
-- input { [1, 2] }
-- output { [2, 3, 0, 1] }
-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 2 }

import "/futlib/array"

let main (ns: []i32): []i32 =
  let t0 = map (+ 1) ns

  -- Create an array whose memory block allocation depends on the *value* of t0,
  -- not the *shape* of t0.  This makes it impossible to hoist the alloc up
  -- before the t0 creation.
  let annoying = iota t0[0]

  -- Try to coalesce t0 and annoying into t2.  Only annoying can be coalesced.
  -- t0 cannot be coalesced, since the allocation of the memory of t2 can only
  -- occur after knowing the value of t0[0].
  let t2 = concat t0 annoying
  in t2
