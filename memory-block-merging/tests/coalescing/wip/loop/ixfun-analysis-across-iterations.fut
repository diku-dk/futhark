-- Limitations in certain loop patterns.
-- ==
-- input { [1, 2] }
-- output { [3, 4] }

-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 1 }

let main [n] (a0: [n]i32): []i32 =
  let a1 = loop b0 = a0 for _i < n do
    let b1 = map (+ 1) b0
    -- The b1 memory currently cannot be coalesced into the generated double
    -- buffer memory because b1 and b0 can be in the same memory block due to
    -- loop cycles.  We do have an index analysis that finds the cases where
    -- writing and reading happens in the same indices, but this analysis does
    -- not work across iterations right now, which is why this program will end
    -- up with two allocations instead of one.
    in b1
  in a1
