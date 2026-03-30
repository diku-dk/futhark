-- ==
-- input { [0i64,1i64]
--         [[[1i64,2i64,3i64],[4i64,5i64,6i64],[7i64,8i64,9i64]],
--          [[10i64,11i64,12i64],[13i64,14i64,15i64],[16i64,17i64,18i64]]]
--         2i64 }
-- auto output

let main [k] (is: [k]i64) (xss: [k][][]i64) (r: i64) =
  map2 (\i xs ->
          let block = xs[i:, i:r]
          let colsums = map i64.sum (transpose block)
          in colsums[0])
       is xss
