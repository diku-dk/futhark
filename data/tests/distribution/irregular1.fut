-- Originally written by Rasmus for flattening - this one exposes
-- really subtle irregularity.
--
-- ==
-- tags { no_opencl }
-- input {
--   [ [ [ [1,2,3], [4,5,6] ]
--     ]
--   , [ [ [6,7,8], [9,10,11] ]
--     ]
--   , [ [ [3,2,1], [4,5,6] ]
--     ]
--   , [ [ [8,7,6], [11,10,9] ]
--     ]
--   ]
--   [3,3,3,3]
-- }
-- output {
--   [[[[2, 4, 6],
--      [8, 10, 12]]],
--    [[[12, 14, 16],
--      [18, 20, 22]]],
--    [[[6, 4, 2],
--      [8, 10, 12]]],
--    [[[16, 14, 12],
--      [22, 20, 18]]]]
-- }
-- structure distributed { }

fun [int] addRows ([int] xs, [int] ys) =
  zipWith(+, xs, ys)

fun [[[[int]]]] main ([[[[int]]]] xssss, [int] cs) =
  zipWith(fn [[[int]]] ([[[int]]] xsss, int c) =>
            let yss = reshape ( (2,c), xsss ) in
            map (fn [[int]] ([[int]] xss) =>
                   zipWith(fn [int] ([int] xs, [int] ys) =>
                             -- An implicit reshape will go here that
                             -- cannot be distributed - this messed up
                             -- the compiler.
                             addRows(xs,ys)
                          , xss, yss)
                , xsss)
         , xssss, cs)
