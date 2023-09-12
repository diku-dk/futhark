def main [n][m] (xss: [n][m]i64) : [n]i64 =
  map (foldl (+) 0) xss

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5204 => [
--     xss_5144
--       [ gtid_5205 | ν par ] [ xss_5144 | ν seq ]
--   ]
