-- Here is a chain of potential coalescings, but only the second one can work, so
-- we can only remove one allocation.
-- ==
-- input { [[2, 2], [2, 2]]
--         [2, 4]
--         0
--         1
--       }
-- output { [[[4, 4], [3, 5]], [[1, 1], [1, 1]]]
--          [[7, 7], [7, 7]]
--        }
-- structure seq-mem { Alloc 3 }
-- structure gpu-mem { Alloc 4 }

def main [n]
         (vss0: [n][n]i32)
         (ns: [n]i32)
         (i: i32)
         (j: i32) : ([n][n][n]i32, [n][n]i32) =
  -- Create arrays into which other arrays can be coalesced.
  let wsss = replicate n (replicate n (replicate n 1))
  let vss = map (\vs -> map (+ 2) vs) vss0
  -- Create the "base" array.
  let xs = map (+ 1) ns
  -- Use vss after the creation of xs.  To ensure that use_vss does not get
  -- hoisted (who knows?), also use a value from the previous array in the
  -- expression.
  let k = xs[0]
  let use_vss = map (\vs -> map (+ k) vs) vss
  -- xs cannot be coalesced into vss[j], since vss is used (in use_vss) after
  -- the creation of xs and before this expression.  If we performed this
  -- coalescing, the use_vss expression would use some values from xs instead of
  -- only values from vss.
  let vss[j] = xs
  -- vss can be coalesced into wsss[i].
  let wsss[i] = vss
  in (wsss, use_vss)
