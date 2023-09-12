def main [n][m] (xss: [n][m]i64) : [m]i64 =
  #[unsafe]
  map (\x -> x*2) xss[0]

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5146 => [
--     xss_5111
--       [ 0i64 | ψ ] [ gtid_5147 | ν par ]
--   ]
