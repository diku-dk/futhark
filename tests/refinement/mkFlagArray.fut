let mkFlagArray 't [m]
        (zero: t)
        (shape: {[m]i64 | \shp -> forall shp (>= 0)})
        (xs: [m]t) : {[]t | \res-> is_indexfn res} =
  let shp_rot = map (\ i -> if i==0 then 0 else shape[i-1]) (iota m)
  let shp_scn = scan (+) 0i64 shp_rot
  let aoa_len = if m > 0 then shp_scn[m-1] + shape[m-1] else 0
  let shp_ind =
        map2 (\ shp ind ->
                if shp <= 0 then -1 else ind
             ) shape shp_scn
  -- let shp_ind =
  --       map2 (\ shp ind ->
  --               if shp == 0 then aoa_len else ind
  --            ) shape shp_scn
  let zeros = replicate aoa_len zero
  let res = scatter zeros shp_ind xs
  in  res
