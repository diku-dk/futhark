-- Here is a chain of potential coalescings, but only the first one can work, so
-- we can only remove one allocation.
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
  -- Create arrays into which other arrays can be coalesced.
  let wsss = map (\wss -> map (\ws -> map (+ 1) ws) wss) wsss0
  let vss = replicate n (replicate n 2)
  -- Create the "base" array.
  let xs = map (+ 1) ns
  -- Use wsss after the creation of vss.  To ensure that use_wsss does not get
  -- hoisted (who knows?), also use a value from the previous array in the
  -- expression.
  let k = xs[0]
  let use_wsss = map (\wss -> map (\ws -> map (+ k) ws) wss) wsss
  -- xs can be coalesced into vss[j].
  let vss[j] = xs
  -- vss (and xs) cannot be coalesced into wsss[i], since wsss is used (in
  -- use_wsss) after the creation of vss and before this expression.  If we
  -- performed this coalescing, the use_wsss expression would use some values
  -- from vss instead of only values from wsss.
  let wsss[i] = vss
  in (wsss, use_wsss)
