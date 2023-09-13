def main [n][m] (xss: [n][m]i64) : i64 =
  #[unsafe]
  foldl (+) 0 xss[0]

-- === Expected output of analysis:
-- entry_main
--   defunc_0_foldl_res_5140 => [
--     defunc_0_foldl_res_dev_5145
--       [ 0i64 | ψ ]
--   ]
--   defunc_0_foldl_res_dev_5145 => [
--     xss_5120
--       [ 0i64 | ψ ] [ i_5141 | ν seq ]
--     acc_5147
--       [ 0i64 | ψ ]
--   ]
