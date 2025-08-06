-- A more complex example.
--
-- This is an example where too aggressive allocation hoisting is a bad thing.
-- If implemented without the current limiter, this would happen: To enable an
-- eventual coalescing into vss, both the allocation for the memory of vss,
-- *and* the vss array creation statement itself, are hoisted upwards as much as
-- possible.  This hinders the later coalescing into wsss, and was never useful
-- to begin with, since there can be no coalescing into vss regardless.
--
-- To fix this in a nicer way, we could perform allocation hoisting while we do
-- coalescing instead of before we do it, which would help at least for this
-- program.  Maybe?
-- ==
-- input { [[[1, 1], [1, 1]], [[1, 1], [1, 1]]]
--         [2, 4]
--         0
--         1
--       }
-- output { [[[2, 2], [3, 5]], [[2, 2], [2, 2]]]
--          [[[5, 5], [5, 5]], [[5, 5], [5, 5]]]
--        }
-- structure seq-mem { Alloc 5 }
-- structure gpu-mem { Alloc 5 }

def main [n]
         (wsss0: [n][n][n]i32)
         (ns: [n]i32)
         (i: i32)
         (j: i32) : ([n][n][n]i32, [n][n][n]i32) =
  -- Create an array into which other arrays can be coalesced.
  let wsss = map (\wss -> map (\ws -> map (+ 1) ws) wss) wsss0
  -- Create the "base" array.
  let xs = map (+ 1) ns
  -- Use wsss after the creation of xs, but *before* the creation of vss.  We
  -- want to consider the case where either xs can be coalesced into vss[j],
  -- *or* vss can be coalesced into wsss[i], but that both are not possible, so
  -- that the compiler will have to make a choice.
  --
  -- If xs is coalesced into vss[j], vss will then alias xs, and so wsss will be
  -- used after an array creation in the memory of vss -- this will not happen
  -- if xs is not coalesced, since then the first array creation in the memory
  -- of vss will occur *after* use_wsss.
  let k = xs[0]
  let use_wsss = map (\wss -> map (\ws -> map (+ k) ws) wss) wsss
  -- Create another coalescing-enabling array.
  let vss = replicate n (replicate n 2)
  -- xs cannot be coalesced into vss[j], since vss is used (in the previous
  -- statement) after the creation of xs.  This is where we see that we cannot
  -- end up with the ambigious case that we were trying to provoke: Since we
  -- want the coalescing into wsss[i] to fail only if the coalescing into vss[j]
  -- succeeds (and to otherwise succeed), we have to create wsss between xs and
  -- vss, in which case vss will be created (and thus used) after xs, which
  -- means that xs cannot even be coalesced into vss[j] in the first place.
  let vss[j] = xs
  -- vss can be coalesced into wsss[i].
  let wsss[i] = vss
  in (wsss, use_wsss)

-- For safety condition 3: This shows that it is always okay (FIXME: proper
-- proof) to optimistically coalesce two memory blocks through a top-down
-- traversal, since that will never restrict later coalescings.  Try proof by
-- contradiction?
