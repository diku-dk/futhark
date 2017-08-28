-- This is a negative test of the reuse pass.  It depends on running the
-- coalescing pass first.
-- ==
-- input { [1, 2]
--       }
-- output { [10, 11]
--        }
-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 2 }


import "/futlib/array"

-- Actual memory blocks:
--   ns: ns_mem (from a parameter, not an allocation)
--   ys: ys_mem
--   zs: iteration_result_mem
--
-- Existential memory blocks:
--   xs_mem, aliases ns_mem AND iteration_result_mem, the latter specifically
--   because of the loop

let main (ns: [#n]i32): [n]i32 =
  let loop_result = loop xs = ns for i < n do
    -- Create a new array using ys_mem.
    --
    -- We add '+ 1' to avoid the expression being a no-op in the first iteration
    -- when i = 0.
    let ys = map (+ i + 1) ns

    -- Use xs and ys to indicate the last uses of xs_mem and ys_mem.
    let k = xs[i] + ys[i]

    -- Create a new array.  After coalescing, this will use
    -- iteration_result_mem.  There has been one array creations so far:
    -- 'map (+ i + 1) ns' in ys_mem.  Naively we might think that it is okay
    -- to allocate into ys_mem, since that seemingly does not interfere.
    --
    -- However, xs_mem aliases iteration_result_mem, since zs is the value of xs
    -- in the next iteration, so by proxy they do interfere.
    let zs = map (+ k) ns

    -- This copy will be inserted by the compiler automatically if we don't
    -- include it, so we do it for clarity.  When the coalescing pass is run,
    -- this will be coalesced.
    let iteration_result = copy zs
    in iteration_result

  -- Use xs0 here to make sure zs_mem is not just allocated into xs0_mem, which
  -- otherwise would be fine, but might deter it from trying to use ys_mem
  -- (which is what we want it to try and then avoid).
  let final_result = loop_result
  in final_result

-- In conclusion, all memory blocks should interfere.  If the aliasing is done
-- naively, zs_mem should not interfere with ys_mem.
