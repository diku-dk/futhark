def main [l][n][m] (xsss: [l][n][m]i64) : i64 =
    let k = 0i64
    in #[unsafe] xsss[k,1,2]

-- === Expected output of analysis:
-- entry_main
--   main_res_5101 => [
--     xsss_5095
--       [ 0i64 | ψ ] [ 1i64 | ψ ] [ 2i64 | ψ ]
--   ]
