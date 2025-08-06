-- An example of a program with silly limits.
-- ==
-- input { [1i64, 2i64] }
-- output { [2i64, 3i64, 0i64, 1i64] }
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 2 }

def main (ns: []i64) : []i64 =
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
