-- Negative test.  'xs' is used while 'ys' is live, so we cannot merge their
-- memory blocks, since 'zs' would then map over the contents of 'ys' instead of
-- the original contents of 'xs[i]'.
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
--          3
--        }
-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 2 }

let main (xs: *[#n][#n]i32, ys0: [#n]i32, i: i32): ([n][n]i32, i32) =
  let ys = map (+ 1) ys0
  let zs = map (+) xs[i] ys -- Cannot be hoisted to exist before 'ys', which
                            -- would have solved the problem.
  let xs[i] = ys
  in (xs, zs[i])
