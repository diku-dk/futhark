-- Test that a map containing a (variant) replicate becomes a fully
-- parallel kernel, with no replicate.
--
-- ==
-- input { [1,2,3] 2 }
-- output { [[1,1], [2,2], [3,3]] }
-- structure distributed { SegMap 1 }

let main [n] (xs: [n]i32) (m: i32): [n][m]i32 =
  map (\(x: i32): [m]i32  ->
        replicate m x) xs
