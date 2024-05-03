let mkFlagArray 't [m]
        (zero: t)
        (shape: {[m]i64 | \shp -> forall shp (>= 0)})
        (xs: [m]t) : {[]t | \res-> is_indexfn res} =
  let shp_rot = map (\ i -> if i==0 then 0 else shape[i-1]) (iota m)
  let shp_scn = scan (+) 0i64 shp_rot
  let aoa_len = shp_scn[m-1] + shape[m-1] -- if m > 0 cond
  let shp_ind =
        map2 (\ shp ind ->
                if shp <= 0 then -1 else ind
             ) shape shp_scn
  let zeros = replicate aoa_len zero
  let res = scatter zeros shp_ind xs
  in  res

let sgmSumInt [n] (flg : [n]i64) (arr : [n]i64) : [n]i64 =
  let flgs_vals = 
    scan ( \ (f1, x1) (f2,x2) -> 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

let part2indicesL 't [n] [m]
                -- the shape of condsL is also shp
                (condsL: [n]bool) (dummy: t)
                (shp: [m]i64, arr: [n]t) :
                ([m]i64, [n]t) =
  let begs   = scan (+) 0 shp --- prepend 0 here; prepend 0 to shp
  let flags  =  (  iota m
                |> map (+1)
                |> mkFlagArray 0i64 shp
                ) :> [n]i64

  let outinds= map (\f -> if f==0 then 0 else f-1) flags
            |> sgmSumInt flags

  let tflgsL = map (\c -> if c then 1i64 else 0i64) condsL
  let fflgsL = map (\b -> 1 - b) tflgsL

  let indsTL = sgmSumInt flags tflgsL
  let tmpL   = sgmSumInt flags fflgsL

  -- let lst = indsT[n-1]
  let lstL   = map2 (\s b -> if s==0 then -1 else #[unsafe] indsTL[b-1]
                    ) shp begs

  -- let indsF = map (+lst) tmp
  let indsFL = map2 (\t sgmind-> t + #[unsafe] lstL[sgmind]) tmpL outinds

  let indsL = map4(\c indT indF sgmind->
                        let offs = if sgmind > 0 then #[unsafe] begs[sgmind-1] else 0i64
                        in  if c then offs + indT - 1
                                 else offs + indF - 1
                  ) condsL indsTL indsFL outinds

  let fltarrL = scatter (replicate n dummy) indsL arr
  in  (lstL, fltarrL)
