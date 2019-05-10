let main (as: [100]i32) (bs: [100]i32) (is: [4]i32) (xsss : [][][]f32) =
  unsafe
  map(\xss ->
        let (ysss, zsss) =
          unzip <|
          map(\xs ->
                let foo =
                  reduce (\i j -> if xs[i] < xs[j]
                                  then i else j)
                        0
                        is
                in (replicate 12 (replicate 12 foo),
                    replicate 12 (replicate 12 xs[0])))
             xss
        let vss =
          map2 (\a b ->
                  map (\zss -> zss[a:a+3, b:b+3] |> flatten)
                      zsss
                  |> flatten)
               as bs
        in (ysss, vss))
     xsss
