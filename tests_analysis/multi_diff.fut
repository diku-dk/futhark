def main [l][n][m] (xsss: [l][n][m]i64) : [l]i64 =
    map2 (\xss i -> #[unsafe] xss[0,i]) xsss (iota l)

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5384 => [
--     xsss_5302
--       [ gtid_5385 | ν par ] [ 0i64 | ψ ] [ gtid_5385 | ν par ]
--   ]
