-- Depending on which branch you take, 'xs' can be thought of to have different
-- live intervals, kind of.  The important thing is that the 'zs' in the
-- then-branch is allowed to use the memory of 'xs'.  A naive implementation
-- might declare that the live interval of 'xs' ends after 'ys', but that is too
-- conservative to handle this case.
--
-- FIXME: The last use analysis does not consider this issue right now.  To do
-- so, it would need something like conditional last uses: For example, the 'k'
-- statement would include the last use of mem_xs with an attribute "this is
-- only true if you have taken the 'then' branch in the if expression whose
-- result is stored in 'ys'".  Somewhat roundabout, but should work.
-- ==
-- input { [2, 5]
--         true
--         0
--       }
-- output { [3, 3]
--        }

-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 1 }

let main (xs0: [#n]i32, cond: bool, i: i32): [n]i32 =
  let xs = map (+ 1) xs0
  let k = xs[i]
  let ys =
    if cond
    then let zs = replicate n k
         in zs
    else let zs = xs
         in zs
  in ys
