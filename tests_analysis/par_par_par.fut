def main [l][n][m] (xsss: [l][n][m]i64) : [l][n][m]i64 =
  map (\xss ->
    map (\xs -> map (+2) xs) xss
  ) xsss

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5485 => [
--     xsss_5281
--       [ gtid_5486 | ν par ] [ gtid_5487 | ν par ] [ gtid_5488 | ν par ]
--   ]
