def main [l][n][m] (xsss: [l][n][m]i64) : [l][n]i64 =
  map (\xss -> 
    map (foldl (+) 0) xss
  ) xsss

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5381 => [
--     xsss_5238
--       [ σ gtid_5382 | ν par ]  [ σ gtid_5383 | ν par ]  [ σ xsss_5238 | ν seq ]
--   ]