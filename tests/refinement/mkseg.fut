let mkseg [m]
      (shape: {[m]i64 | \shp -> forall shp (>= 0)})
      : {([]bool, []i64) | \res-> is_indexfn res} =
  -- mkFlagArray
  -- let flags1 = map (+1) (iota m)
  -- let flags  = mkFlagArray 0i64 shp flags1
  let shp_rot = map (\ i -> if i==0 then 0 else shape[i-1]) (iota m)
  let shp_scn = scan (+) 0i64 shp_rot
  let aoa_len = shp_scn[m-1] + shape[m-1] -- if m > 0 cond
  let shp_ind =
        map2 (\ shp ind ->
                if shp <= 0 then -1 else ind
             ) shape shp_scn
  let flags = scatter (replicate aoa_len false) shp_ind (replicate m true)
  -- let seg0 = map (\f -> if f==0 then 0 else f-1) flags
  let seg0 = map2 (\f i -> if f then i else 0) flags (iota aoa_len)
  let zipped = zip flags seg0
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_, seg) = unzip flags_ys
  in  (flags, seg)

-- entry point without refinement annotations
entry main [m]
      (shapei32: [m]i32)
      : ([]bool, []i64) =
  -- mkFlagArray
  -- let flags1 = map (+1) (iota m)
  -- let flags  = mkFlagArray 0i64 shp flags1
  let shape = map i64.i32 shapei32
  let shp_rot = map (\ i -> if i==0 then 0 else shape[i-1]) (iota m)
  let shp_scn = scan (+) 0i64 shp_rot
  let aoa_len = shp_scn[m-1] + shape[m-1] -- if m > 0 cond
  let shp_ind =
        map2 (\ shp ind ->
                if shp <= 0 then -1 else ind
             ) shape shp_scn
  let flags = scatter (replicate aoa_len false) shp_ind (replicate m true)
  -- let seg0 = map (\f -> if f==0 then 0 else f-1) flags
  let seg0 = map2 (\f i -> if f then i else 0) flags (iota aoa_len)
  let zipped = zip flags seg0
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_, seg) = unzip flags_ys
  in  (flags, seg)
