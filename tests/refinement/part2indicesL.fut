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

let partition2L 't [m]
                (dummy: i64)
                (shape2: {[m]i64 | \shp -> forall shp (>= 0)})
                (n: {i64 | \n' -> n' == sum shape2})
                -- `shape` is the shape of condsL and arr
                (condsL: [n]bool)
                (arr: [n]i64) : {[n]i64 | \res-> is_indexfn res} =
  let begs  = scan (+) 0 shape2
  -- n is only passed because `:> [n]bool` doesn't work
  -- let flags = mk_flag_array 0i64 shape2 (replicate m 1i64) n
  let flags = mk_flag_array 0i64 shape2 (iota m) n
  in flags

  -- let xs_outinds = map2 (\f i -> if f then i else 0) flags (iota n)
  -- let outinds = sgm_sum flags xs_outinds
  -- in outinds

  -- let tflgsL = map (\c -> if c then 1i64 else 0i64) condsL
  -- let fflgsL = map (\b -> 1 - b) tflgsL

  -- let indsTL = sgmSumInt flags tflgsL
  -- let tmpL   = sgmSumInt flags fflgsL

  -- -- let lst = indsT[n-1]
  -- let lstL   = map2 (\s b -> if s==0 then -1 else #[unsafe] indsTL[b-1]
  --                   ) shp begs

  -- -- let indsF = map (+lst) tmp
  -- let indsFL = map2 (\t sgmind-> t + #[unsafe] lstL[sgmind]) tmpL outinds

  -- let indsL = map4(\c indT indF sgmind->
  --                       let offs = if sgmind > 0 then #[unsafe] begs[sgmind-1] else 0i64
  --                       in  if c then offs + indT - 1
  --                                else offs + indF - 1
  --                 ) condsL indsTL indsFL outinds

  -- let fltarrL = scatter (replicate n dummy) indsL arr
  -- in  (lstL, fltarrL)
