def segmented_sum [n][m] 't
      (shape: {[m]i64 | \shp -> forall shp (>= 0)})
      (xs: [n]i64): {[n]i64 | \res-> is_indexfn res} =
  -- mkFlagArray
  let shp_rot = map (\ i -> if i==0 then 0 else shape[i-1]) (iota m)
  let shp_scn = scan (+) 0i64 shp_rot
  let shp_ind =
        map2 (\ shp ind ->
                if shp <= 0 then -1 else ind
             ) shape shp_scn
  let flags = scatter (replicate n false) shp_ind (replicate m true)
  -- segmented_sum
  let zipped = zip flags xs
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_flags, ys) = unzip flags_ys
  in ys
