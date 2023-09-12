-- Make sure 'i*0' reduces to ψ
def main [n] (xs: [n]i64) : [n]i64 =
  map (\i ->  #[unsafe] xs[i*0] ) (iota n)

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5193 => [
--     defunc_0_map_res_5198
--       [ defunc_0_map_res_5198 | ν par ] [ 0i64 | ψ ]
--   ]
