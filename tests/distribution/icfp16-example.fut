-- This is the program used as a demonstration example in our paper
-- for ICFP 2016.
--
-- ==
-- input {
--   [[1,2,3],[3,2,1],[4,5,6]]
-- }
-- output {
--   [[[1i32, 2i32, 3i32],
--     [2i32, 3i32, 4i32],
--     [5i32, 6i32, 7i32]],
--    [[7i32, 6i32, 5i32],
--     [4i32, 3i32, 2i32],
--     [3i32, 2i32, 1i32]],
--    [[14i32, 15i32, 16i32],
--     [24i32, 25i32, 26i32],
--     [39i32, 40i32, 41i32]]]
--   [[92i32, 142i32, 276i32],
--    [276i32, 142i32, 92i32],
--    [662i32, 1090i32, 1728i32]]
-- }
-- structure distributed {
--   DoLoop/Kernel 1
--   Kernel 7
-- }

let main [n][m] (pss: [n][m]i32): ([n][m][m]i32, [n][m]i32) =
  let (asss, bss) =
    unzip(map (\(ps: []i32): ([m][m]i32, [m]i32)  ->
                let ass = map (\(p: i32): [m]i32  ->
                                let cs = scan (+) 0 (0..1..<p)
                                let f = reduce (+) 0 cs
                                let as = map (+f) ps
                                in as) ps
                let bs' = loop bs=ps for i < n do
                  let bs' = map (\(as: []i32, b: i32): i32  ->
                                  let d = reduce (+) 0 as
                                  let e = d + b
                                  let b' = 2 * e
                                  in b') (
                                zip ass bs)
                  in bs'
                in (ass, bs')) pss)
  in (asss, bss)
