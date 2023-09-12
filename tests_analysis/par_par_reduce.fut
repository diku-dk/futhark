def main [l][n][m] (xsss: [l][n][m]i64) : [l][n]i64 =
  map (\xss ->
    map (reduce (+) 0) xss
  ) xsss

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5336 => [
--     xsss_5239
--       [ gtid_5458 | ν par ] [ gtid_5459 | ν par ] [ gtid_5460 | ν par ]
--       [ gtid_5442 | ν par ] [ gtid_5443 | ν par ] [ i_5477 | ν seq ]
--       [ gtid_5362 | ν par ] [ i_5472 | ν seq ] [ i_5475 | ν seq ]
--   ]
