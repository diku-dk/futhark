-- Positive test.  Similar to neg0.fut, except this one will work if the zs
-- statement is hoisted to before 'ys'.
--
-- Disabled because it fails with in-place-lowering (not part of this
-- optimisation suite).
-- ==
-- tags { disable }
-- input { [[2, 2],
--          [2, 2]]
--         [3, 4]
--         1
--       }
-- output { [[2, 2],
--           [4, 5]]
--          6
--        }
-- structure cpu { Alloc 1 }

let main (xs: *[#n][#n]i32, ys0: [#n]i32, i: i32): ([n][n]i32, i32) =
  let ys = map (+ 1) ys0 -- Will use the memory of xs[i].
  let zs = map (+ 1) xs[i] -- Will be moved
  let xs[i] = ys
  in (xs, zs[i]) -- Make sure zs is not simplified away.
