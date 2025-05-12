-- Copied from prelude/array.fut to infer index function.
def length [n] 't (_: [n]t) = n

def sum_i64 [n] (xs: [n]i64) = if n > 0 then (scan (+) 0 xs)[n-1] else 0

def mk_flag_array_exclusive 't [m]
        (zero: t)
        (shape: [m]{i64 | (>= 0) })
        (xs: [m]t) : {(i64, []t) | \(_, flags) -> length flags == sum_i64 shape} =
  let shp_scn = scan (+) 0i64 shape
  let aoa_len = if m > 0 then shp_scn[m-1] else 0
  let aoa_len = if m > 0 then shp_scn[m-1] + shape[m-1] else 0
  let shp_ind =
        map2 (\ shp ind ->
                if shp <= 0i64 then -1i64 else ind
             ) shape shp_scn
  let zeros = replicate aoa_len zero
  let flags = scatter zeros shp_ind xs
  in (aoa_len, flags)
