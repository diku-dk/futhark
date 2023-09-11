def main [n][m] (xss: [n][m]i64) : [m]i64 =
    foldl (\acc xs -> map2 (\a x -> x+a) acc xs) (replicate m 0) xss

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5425 => [
--     xsss_????
--       [ σ xss_5346 | ν seq ] [ σ xss_5346 | ν par ]
--   ]

-- WARNING: this output might be wrong!!!
