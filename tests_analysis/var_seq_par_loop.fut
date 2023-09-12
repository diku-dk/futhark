def main [n][m] (xss: [n][m]i64) : [m]i64 =
    #[unsafe]
    loop res=replicate m 0 for i < n
    do
        map2 (\x r -> x+r) xss[i] res

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5323 => [
--     xsss_????
--       [ xss_5346 | ν seq ] [ xsss_5223 | ν par ]
--   ]

-- WARNING: this output might be wrong!!!
