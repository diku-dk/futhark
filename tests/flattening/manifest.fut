-- ==
-- input { [1i64, 2i64, 4i64]
--         [[1i64, 2i64], [3i64, 4i64], [5i64, 6i64]] }
-- auto output
-- structure gpu { SegScan 1 /Apply/segiota 1 }

entry main (ns: []i64) (xss: [][]i64) : []i64 =
  let irregular =
    #[incremental_flattening(only_inner)]
    map (\n ->
           let xs = manifest (iota n)
           in i64.sum xs)
        ns
  let regular =
    #[incremental_flattening(only_inner)]
    map (\xs ->
           let ys = manifest xs
           in i64.sum ys)
        xss
  in irregular ++ regular
