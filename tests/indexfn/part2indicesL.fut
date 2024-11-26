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


-- Expands a shape array to flat arrays of segment ids and flags.
let segment_ids [m]
      (shape: {[m]i64 | elementwise (>= 0)})
      (n: {i64 | equals (sum_i64 shape)})
      : ([n]i64, [n]bool) =
  let flags1 = map (\i -> i + 1) (iota m)
  let flags = mk_flag_array shape n 0i64 flags1
  let flags_sgmind = map (\f -> if f == 0 then 0 else f-1) flags
  let flags_bool = map (\f -> f > 0) flags
  in (sgm_sum flags_bool flags_sgmind, flags_bool)

let part2indicesL 't [m]
                (shape: {[m]i64 | elementwise (>= 0)})
                (n: {i64 | equals (sum_i64 shape)})
                -- `shape` is the shape of csL
                (csL: [n]bool)
                : [n]i64 =
  let (seg_ids, flags) = segment_ids shape n

  let tflgs = map (\c -> if c then 1i64 else 0i64) csL
  let fflgs = map (\b -> 1 - b) tflgs

  let indsT = sgm_sum flags tflgs
  let tmp   = sgm_sum flags fflgs
  let begs  = scan (+) 0 shape

  -- let lst = indsT[n-1]
  let lst   = map2 (\s b -> if s==0 then -1 else #[unsafe] indsT[b-1]
                   ) shape begs

  -- -- let indsF = map (+lst) tmp
  let indsF = map2 (\t sgmind-> t + #[unsafe] lst[sgmind]) tmp seg_ids

  let offs = map (\segi -> if segi > 0 then begs[segi-1] else 0i64) seg_ids
  let inds = map4(\c indT indF offset ->
                      if c then offset + indT - 1
                           else offset + indF - 1
                  ) csL indsT indsF offs
  in inds
