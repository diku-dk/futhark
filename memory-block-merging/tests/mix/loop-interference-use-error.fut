-- This is similar to loop-interference-use.fut, except the valid reuse in that
-- test program is *invalid* in this program, because the actual memory block is
-- used after the existential memory block that aliases it.
--
-- This test checks that the compiler does not use aliases naively.
-- ==
-- input { [1, 2]
--       }
-- output { [31, 32]
--        }
-- structure cpu { Alloc 4 }


import "/futlib/array"

let main (ns: [#n]i32): [n]i32 =
  let xs0 = map (+ 1) ns
  let loop_result = loop xs = xs0 for i < n do
    let ys = map (+ 1) xs
    let k = ys[i]

    -- ts0 cannot reuse xs0_mem, since the last use of xs0_mem is in k0.  If
    -- implemented poorly, the compiler might say that the last use of xs0_mem
    -- is in the last use of xs_mem, for the same reasons as in
    -- loop-interference-use.fut.
    let ts0 = map (+ k) ns

    -- The last use of xs0_mem.  This will also count as the last use of xs_mem,
    -- since this *might* (and the compiler cannot know statically) be an
    -- iteration where xs_mem refers to the same memory as xs0_mem.
    --
    -- Pitfall: While this should be the last use of both xs_mem and xs0_mem, it
    -- should *not* be the last use of any other memory block aliased to by
    -- xs_mem (i.e. double_buffer_mem).
    let k0 = ys[n - i - 1] + ts0[i] + xs0[i]

    let ts1 = map (+ k0) ns
    in ts1
  in loop_result
