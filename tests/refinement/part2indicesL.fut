let part2indicesL [n] [m]
                -- the shape of condsL is also shp
                (condsL: [n]bool)
                (dummy: i64)
                (shape: {[m]i64 | \shp -> forall shp (>= 0)})
                (arr: [n]i64) : {[n]i64 | \res-> is_indexfn res} =
  -- mkFlagArray
  let shp_rot = map (\ i -> if i==0 then 0 else shape[i-1]) (iota m)
  let shp_scn = scan (+) 0i64 shp_rot
  let shp_ind =
        map2 (\ shp ind ->
                if shp <= 0 then -1 else ind
             ) shape shp_scn
  let flags = scatter (replicate n false) shp_ind (replicate m true)
  -- let outinds= map (\f -> if f==0 then 0 else f-1) flags
  --           |> sgmSumInt flags
  let xs_outinds = map2 (\f i -> if f then i else 0) flags (iota n)
  let zipped = zip flags xs_outinds
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_flags, outinds) = unzip flags_ys
  -- in outinds

  let tflgsL = map (\c -> if c then 1i64 else 0i64) condsL
  let fflgsL = map (\b -> 1 - b) tflgsL

  -- let indsTL = sgmSumInt flags tflgsL
  let zipped = zip flags tflgsL
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_flags, indsTL) = unzip flags_ys
  -- in indsTL

  -- let tmpL   = sgmSumInt flags fflgsL
  let zipped = zip flags fflgsL
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_flags, tmpL) = unzip flags_ys
  in tmpL

  -- -- let lst = indsT[n-1]
  -- let begs   = scan (+) 0 shape --- prepend 0 here; prepend 0 to shp
  -- let lstL   = map2 (\s b -> if s==0 then -1 else #[unsafe] indsTL[b-1]
  --                   ) shape begs

  -- -- let indsF = map (+lst) tmp
  -- let indsFL = map2 (\t sgmind-> t + #[unsafe] lstL[sgmind]) tmpL outinds
  -- in indsFL

  -- let indsL = map4(\c indT indF sgmind->
  --                       let offs = if sgmind > 0 then #[unsafe] begs[sgmind-1] else 0i64
  --                       in  if c then offs + indT - 1
  --                                else offs + indF - 1
  --                 ) condsL indsTL indsFL outinds

  -- let fltarrL = scatter (replicate n dummy) indsL arr
  -- in  fltarrL
