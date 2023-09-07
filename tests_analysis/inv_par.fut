def main [n][m] (xss: [n][m]i64) : [m]i64 = 
  #[unsafe]
  map (\x -> x*2) xss[0]

-- === Expected output of analysis:
-- entry_main
--   segmap_usable_groups_5145

--   eta_p_5149
--     xss_5111 [[τ 0i64 | ψ], [σ gtid_5147 | ν | par]]
--   lifted_lambda_res_5150