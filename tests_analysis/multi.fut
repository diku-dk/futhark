def main [l][n][m] (xsss: [l][n][m]i64) : i64 =
    #[unsafe] xsss[0,1,2]

-- === Expected output of analysis:
-- entry_main
--   main_res_5102 [[τ 0i64 | ψ | seq], [τ 1i64 | ψ | seq], [τ 2i64 | ψ | seq]]