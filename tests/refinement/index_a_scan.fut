-- Make sure that aoa_len is scalar (has empty domain).
let mkFlagArray 't [m]
        (zero: t)
        (aoa_shp: [m]i64)
        (aoa_val: [m]t) : {i64 | \res-> is_indexfn res} =
  let iota_m = iota m
  let shp_rot = map (\ i -> if i==0 then 0 else aoa_shp[i-1]) iota_m
  let shp_scn = scan (+) 0i64 shp_rot
  let aoa_len = shp_scn[m-1] + aoa_shp[m-1] -- if m > 0 cond
  in aoa_len
