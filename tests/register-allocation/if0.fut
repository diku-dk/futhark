-- Depending on which branch you take, 'xs' can be thought of to have different
-- live intervals, kind of.  The important thing is that the 'zs' in the
-- then-branch is allowed to use the memory of 'xs'.  A naive implementation
-- might declare that the live interval of 'xs' ends after 'ys', but that is not
-- good enough to handle this case.
-- ==

-- input {
--       }
-- output {
--        }
-- structure cpu { Alloc 1 }

let main (xs0: [#n]i32, cond: bool, i: i32): [n]i32 =
  let xs = map (+ 1) xs0
  let ys =
    if cond
    then let k = xs[i]
         let zs = map (+ k) (iota n) -- Can use the memory of 'xs'.
         in zs
    else let zs = replicate n (reduce (+) 0 xs) -- xs
         in zs
  let zs = map (+ 1) ys
  in zs
