let sum_i64 [n] (xs: [n]i64) = if n > 0 then (scan (+) 0 xs)[n-1] else 0

let mk_flag_array 't 'a [m]
        (shape: {[m]i64 | \x -> Range x (0, inf) })
        (n: {i64 | (== sum_i64 shape)})
        (zero: t)
        (xs: [m]t) : {[n]t | \_ -> true} =
  let shp_rot = map (\i -> if i==0 then 0i64 else shape[i-1]) (iota m)
  let shp_scn = scan (+) 0i64 shp_rot
  let shp_ind =
        map2 (\ shp ind ->
                if shp <= 0i64 then -1i64 else ind
             ) shape shp_scn
  -- let aoa_len = shp_scn[m-1] + shape[m-1] -- if m > 0 cond
  let zeros = replicate n zero
  let res = scatter zeros shp_ind xs
  in  res

def sgm_sum [n] 't
      (flags: [n]bool)
      (xs: [n]i64): {[n]i64 | \_ -> true} =
  let zipped = zip flags xs
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_flags, ys) = unzip flags_ys
  in ys


-- Expands a shape array to a flat array of segment ids.
let segment_ids [m]
      (shape: {[m]i64 | \x -> Range x (0, inf) })
      (n: {i64 | (== sum_i64 shape)})
      : {[]i64 | \_ -> true} =
  let flags1 = map (\i -> i + 1) (iota m)
  let zero = 0
  let flags = mk_flag_array shape n zero flags1
  let flags_sgmind = map (\f -> if f == 0 then 0 else f-1) flags
  let flags_bool = map (\f -> f > 0) flags
  in sgm_sum flags_bool flags_sgmind
