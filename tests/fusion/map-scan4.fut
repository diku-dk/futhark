-- The trick behind this program is to force an array to be passed between the
-- pre-lambda and the post-lambda.
-- ==
-- input { 2i64 [1,2,3] }
-- output { [4, 8, 13] }
-- structure { Screma 1 }
-- structure gpu { SegScan 1 }

entry main (m: i64) (xs: []i32) =
  let (as, bs) =
    unzip (map (\x ->
                  ( x
                  , loop tmp = replicate m x
                    for i < m do
                      tmp with [i] = tmp[i] + i32.i64 i
                  ))
               xs)
  let as' = scan (+) 0 as
  in map2 (\a b -> a + b[0] + b[1]) as' bs
