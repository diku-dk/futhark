def main [N] [D] [K] [triD] (x: [N][D]f64) (means: [K][D]f64) (qs: [K][D]f64) (ls: [K][triD]f64) =
  let xs =
    map (\x' ->
           unzip3 (tabulate K (\k ->
                                 ( map2 (-) x' means[k]
                                 , qs[k]
                                 , ls[k]
                                 ))))
        x
  let a = map (.0) xs
  let b = reduce (map2 (map2 (+))) (map (map (const 0)) means) (map (.1) xs)
  let c = reduce (map2 (map2 (+))) (map (map (const 0)) ls) (map (.2) xs)
  in ( a
     , map (map (+ 2)) b
     , c
     )
