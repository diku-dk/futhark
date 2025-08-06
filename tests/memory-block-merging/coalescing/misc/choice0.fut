-- An example of a program where there is a coalescing choice.
-- ==
-- input { [1i64, 2i64] }
-- output { [2i64, 3i64, 0i64, 1i64] }
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 2 }

def main [n] (ns: [n]i64) : []i64 =
  let t0 = map (+ 1) ns
  -- Create an array whose memory block allocation depends on the *value* of t0,
  -- not the *shape* of t0.  This makes it impossible to hoist the alloc up
  -- before the t0 creation.
  let annoying = iota t0[0]
  -- Either coalesce t0 into t1...
  let t1 = copy t0
  -- ... or coalesce t1 into t2.  Both will not work:
  --
  --  + If t0 is coalesced into t1, the allocation of t2 needs to be hoisted way
  --    up to before the creation of t0.  This is not possible due to the
  --    allocation size depending on the size of annoying.
  --
  --  + Else, the allocation of t2 just needs to be hoisted up to before the
  --    creation of t1, which is doable.
  --
  -- Either will work on their own.  annoying can always be coalesced.
  let t2 = concat t1 annoying
  in t2
