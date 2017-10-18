-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   129.6
--   [1.0,-3.0, -5.4]
--   [2.0,-6.0,-10.8]
--   [2.0,-4.0,-14.8]
--   [7.0, 1.0, -9.8]
-- }
-- structure {
--    Stream   0
--    Scan     1
--    Scanomap 1
--    Map      0
--    Redomap  1
-- }
--
let main(arr: []f64): (f64,[]f64,[]f64,[]f64,[]f64) =
    let sa = scan (+) (0.0) arr
    let b  = map  (*2.0) sa
    let sb = scan (+) (0.0) b
    let c  = map  (+5.0) sb
    let r  = reduce (*) (1.0) b in
    (r, sa, b, sb, c)

