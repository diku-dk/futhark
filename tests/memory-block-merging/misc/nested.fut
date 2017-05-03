-- Memory block merging in a sub-body.
-- ==
-- input { 2
--         1
--         true
--       }
-- output { [[0, 1],
--           [10, 11]]
--        }

let main (n: i32, i: i32, cond: bool): [n][n]i32 =
  let nss = replicate n (iota n)
  let result =
    if cond
    then let ms = map (+ 10) (iota n)
         let nss[i] = ms -- This should be coalesced.  It is irrelevant that it
                         -- is inside an 'if'.  The point is that it is a nested
                         -- body.
         in nss
    else nss
  in result
