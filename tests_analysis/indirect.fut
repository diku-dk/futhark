def main [n][m] (is: [m]i64) (xss: [n][m]i64) : [n]i64 =
  map (\xs ->
    #[unsafe]
    loop s=0 for i < m
    do
      s + xs[ is[ i ] ]
  ) xss


-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5231 => [
--     is_5157
--       [ σ i_5236 | ν seq ]
--     xss_5158
--       [ σ gtid_5232 | ν par ] [ σ +_rhs_5238 | ν seq ]
--   ]
