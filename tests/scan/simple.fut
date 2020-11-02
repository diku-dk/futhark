-- ==
-- entry: main
--
-- no_gtx780 no_k40 compiled random input { [10000000]f32 } auto output

let main [n] (xs:[n]f32): [n]f32 =
  scan (+) 0 xs
