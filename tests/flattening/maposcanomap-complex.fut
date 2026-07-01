-- ==
-- input { 3i64 [[1i64,2i64,3i64],[4i64,5i64,6i64]] }
-- auto output

entry main [n] [m] (k: i64) (xss: [n][m]i64) =
  map
    (\xs ->
       let (as, bs) =
         unzip
           ( map
               (\x ->
                  let ys = replicate k x
                  let zs = map (+ x) ys
                  in (x * 2, zs))
               xs
           )
       let as' = scan (+) 0 as
       in map2
            (\a b ->
               let shifted = map (+ a) b
               let scaled =
                 if a > 10
                 then shifted
                 else map (* 2) shifted
               in reduce (+) 0 scaled)
            as'
            bs)
    xss
