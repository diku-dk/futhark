def main [n][m] (xss: [n][m]i64) : i64 = 
  #[unsafe]
  foldl (+) 0 xss[0]

-- === Expected output of analysis:
-- entry_main
--   defunc_0_foldl_res_5140 => [
--     defunc_0_foldl_res_dev_5145
--       [ τ 0i64 | ψ ]  [ σ xss_5144 | ν seq ]
--   ]
