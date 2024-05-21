let mk_flag_array 't 'a [m]
        (zero: t)
        (shape: {[m]i64 | \shp -> forall shp (>= 0)})
        (xs: [m]t)
        (n: {i64 | \n' -> n' == sum shape}): {[n]t | \res-> is_indexfn res} =
  let shp_rot = map (\ i -> if i==0 then 0 else shape[i-1]) (iota m)
  let shp_scn = scan (+) 0i64 shp_rot
  let shp_ind =
        map2 (\ shp ind ->
                if shp <= 0 then -1 else ind
             ) shape shp_scn
  -- let aoa_len = shp_scn[m-1] + shape[m-1] -- if m > 0 cond
  let zeros = replicate n zero
  let res = scatter zeros shp_ind xs
  in  res

def sgm_sum [n] 't
      (flags: [n]bool)
      (xs: [n]i64): {[n]i64 | \res-> is_indexfn res} =
  let zipped = zip flags xs
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_flags, ys) = unzip flags_ys
  in ys

let part2indicesL 't [m]
                (dummy: i64)
                (shp: {[m]i64 | \shp -> forall shp (>= 0)})
                (n: {i64 | \n' -> n' == sum shp})
                -- `shape` is the shape of csL and arr
                (csL: [n]bool)
                (arr: [n]i64) : {[n]i64 | \res-> is_indexfn res} =
  let flags1 = map (\i -> i + 1) (iota m)
  let flags_int = mk_flag_array 0i64 shp flags1 n
  let flags_sgmind = map (\f -> if f == 0 then 0 else f-1) flags_int
  let flags = map (\f -> f > 0) flags_int
  let seg = sgm_sum flags flags_sgmind

  let tflgsL = map (\c -> if c then 1i64 else 0i64) csL
  let fflgsL = map (\b -> 1 - b) tflgsL
  -- in fflgsL

  let indsTL = sgm_sum flags tflgsL -- CHECK
  let tmpL   = sgm_sum flags fflgsL -- CHECK (but different from hand deriv)
  let begs  = scan (+) 0 shp -- CHECK

  -- -- let lst = indsT[n-1]
  let lstL   = map2 (\s b -> if s==0 then -1 else #[unsafe] indsTL[b-1]
                    ) shp begs
              -- CHECK

  -- -- let indsF = map (+lst) tmp
  let indsFL = map2 (\t sgmind-> t + #[unsafe] lstL[sgmind]) tmpL seg
  -- ^CHECK (slightly different from hand deriv due to tmpL)
  -- in indsFL

  let offsL= map (\segi -> if segi > 0 then begs[segi-1] else 0i64) seg
  -- ^CHECK
  let indsL = map4(\c indT indF offset ->
                      if c then offset + indT - 1
                           else offset + indF - 1
                  ) csL indsTL indsFL offsL
  -- ^CHECK
  in indsL

  -- let fltarrL = scatter (replicate n dummy) indsL arr
  -- in  (lstL, fltarrL)
