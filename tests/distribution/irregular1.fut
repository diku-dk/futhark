-- Originally written by Rasmus for flattening - this one exposes
-- really subtle irregularity.
--
-- ==
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

let addRows (xs: []i32, ys: []i32): []i32 =
  map2 (+) xs ys

let main (xssss: [][][][]i32, cs: []i32): [][][][]i32 =
  map2 (\(xsss: [][][]i32) (c: i32): [][][]i32  -> unsafe
            let yss = unflatten 2 c (flatten_3d xsss) in
            map  (\(xss: [][]i32): [][]i32  ->
                   map2 (\(xs: []i32) (ys: []i32): []i32  ->
                             -- An implicit reshape will go here that
                             -- cannot be distributed - this messed up
                             -- the compiler.
                             unsafe addRows(xs,ys)
                          ) xss yss
                ) xsss
         ) xssss cs
