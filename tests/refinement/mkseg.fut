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


let mkseg [m]
      (shape: {[m]i64 | \shp -> forall shp (>= 0)})
      (n: {i64 | \n' -> n' == sum shape})
      : {[]i64 | \res-> is_indexfn res} =
  let flags1 = map (\i -> i + 1) (iota m)
  let flags = mk_flag_array 0i64 shape flags1 n
  let flags_sgmind = map (\f -> if f == 0 then 0 else f-1) flags
  let flags_bool = map (\f -> f > 0) flags
  let outinds = sgm_sum flags_bool flags_sgmind
  in outinds
