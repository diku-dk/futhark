def main [n][m] (xss: [n][m]i64) : i64 = 
  loop res=0 for i < m do
    #[unsafe]
    res + xss[0][i]

-- === Expected output of analysis:
-- entry_main
--   main_res_5098 [[τ 0i64 | ψ ], [σ xss_5144 | ν | seq]]
--   const_dev_5104 []