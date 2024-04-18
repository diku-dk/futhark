let mkFlagArray 't [m]
        (zero: t)
        (aoa_shp: [m]i64)
        (aoa_val: [m]t) : {[]t | \res-> is_indexfn res} =
  let iota_m = iota m
  let shp_rot = map (\ i -> if i==0 then 0 else aoa_shp[i-1]) iota_m
  let shp_scn = scan (+) 0i64 shp_rot
  let aoa_len = shp_scn[m-1] + aoa_shp[m-1] -- if m > 0 cond
  let shp_ind =
        map2 (\ shp ind ->
                if shp < 0 then -1 else ind
             ) aoa_shp shp_scn
  -- let shp_ind =
  --       map2 (\ shp ind ->
  --               if shp == 0 then aoa_len+1 else ind
  --            ) aoa_shp shp_scn
  let zeros = replicate aoa_len zero
  let res = scatter zeros shp_ind aoa_val
  in  res
