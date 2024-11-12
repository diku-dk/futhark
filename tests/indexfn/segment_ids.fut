let sum_i64 [n] (xs: [n]i64) = (scan (+) 0 xs)[n-1]

let mk_flag_array 't 'a [m]
        (shape: {[m]i64 | elementwise (>= 0)})
        (n: {i64 | equals (sum_i64 shape)})
        (zero: t)
        (xs: [m]t) : [n]t =
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
      (xs: [n]i64): [n]i64 =
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
      (shape: {[m]i64 | elementwise (>= 0)})
      (n: {i64 | equals (sum_i64 shape)})
      : []i64 =
  let flags1 = map (\i -> i + 1) (iota m)
  let flags = mk_flag_array shape n 0i64 flags1
  let flags_sgmind = map (\f -> if f == 0 then 0 else f-1) flags
  let flags_bool = map (\f -> f > 0) flags
  in sgm_sum flags_bool flags_sgmind
