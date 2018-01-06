-- This is a somewhat pathological example of how a loop variable can have its
-- last use in the beginning of a loop and its first use in the end of the loop,
-- and how that can be used to reuse memory.  The coalescing pass removes one
-- allocation, and the reuse pass removes another allocation.
-- ==
-- input { [1, 2]
--       }
-- output { [20, 21]
--        }
-- structure cpu { Alloc 3 }
-- structure gpu { Alloc 3 }

import "/futlib/array"

let main [n] (ns: [n]i32): [n]i32 =
  -- An array whose memory we can reuse.
  let xs0 = copy ns
  let loop_result = loop xs = xs0 for i < n do
    -- Use xs_mem.
    let ys = map (+ 1) xs
    -- Avoid fusing the ys and ts0 maps.
    let k = ys[i]
    -- Create an array.  The compiler will try to reuse memory for this array.
    -- It has access to the non-existential memory blocks xs0_mem and ys_mem.
    -- ys_mem is lastly used later, so that cannot be reused.  xs0_mem is
    -- aliased by the existential xs_mem, so conservatively it cannot be used
    -- either, since xs_mem and its aliases can be said to interfere with the
    -- entire loop.  However, since ts1 creates a new array that does not read
    -- from xs_mem, we can instead say that xs_mem has a last use in ys and a
    -- first use in ts1.  This allows the compiler to reuse the aliased xs0_mem
    -- inbetween ys and ts1 (assuming xs0_mem does not have its own direct last
    -- use later on, overriding any inherited last use from the existential
    -- xs_mem).
    let ts0 = map (+ k) ns
    -- Last use of ys_mem and ts0_mem.
    let k0 = ys[n - i - 1] + ts0[i]
    -- ts1 will be coalesced into the generated double_buffer_mem.
    let ts1 = map (+ k0) ns
    in ts1
  in loop_result
