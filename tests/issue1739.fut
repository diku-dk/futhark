entry main [n] [m] (xs: [3][3]f32) (as: *[n][m]f32) =
  let f i =
    let v = filter (> 0) as[i] ++ as[i]
    in map (\x ->
              map (\y ->
                     #[sequential]
                     map (* f32.sum y * f32.sum x) v
                     |> f32.sum)
                  xs)
           xs
       |> map (map (+ (f32.i64 i)))
  let bs =
    #[incremental_flattening(no_intra)]
    tabulate n f
  in bs
