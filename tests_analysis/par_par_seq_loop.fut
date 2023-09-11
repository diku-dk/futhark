def main [l][n][m] (xsss: [l][n][m]i64) : [l][n]i64 =
  map (\xss ->
    map (\xs ->
      #[unsafe]
      loop res=xs[0] for i < n do
        res + xs[i]
    ) xss
  ) xsss

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5390 => [
--     xsss_5241
--       [ σ gtid_5391 | ν par ] [ σ gtid_5392 | ν par ] [ τ 0i64 | ν seq ]
--   ]

-- WARNING: This output may be wrong!!!
