def main [l][n][m] (xsss: [l][n][m]i64) : [l][m]i64 =
  map (\xss ->
    #[unsafe]
    loop _=xss[0] for i < n do
      map (\x -> x*2) xss[i]
  ) xsss

-- === Expected output of analysis:
-- entry_main
--   xsss_transformed_5310 => [
--     xsss_5223
--       [ σ xsss_5223 | ν par ] [ τ 0i64 | ν seq ] [ σ xsss_5223 | ν par ]
--   ]

-- WARNING: This output may be wrong!!
