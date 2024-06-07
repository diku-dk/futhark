-- Based on #2144.
-- ==
-- error: inside the loop body

def hide [m] (aoa_shp: [m]i32) : ?[EXT].[EXT]i32 =
  filter (!=0) aoa_shp

entry main [n] (iv : [n]bool) =
  loop (wrk, wrk_shp) = (iv,  [(i32.i64 n)]) for _i < 2 do
  let flags = hide wrk_shp
  let min = zip flags wrk -- There is no way 'wrk' can have the right shape here.
  in (wrk ++ wrk, wrk_shp ++ wrk_shp)
