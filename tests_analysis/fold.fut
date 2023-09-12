def main [n][m] (xss: [n][m]i64) : [n]i64 =
  map (foldl (+) 0) xss

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5204 => [
--     xss_5144
--       [ gtid_5205 | ν par ] [ 0i64 :+ m_5143 * 1i64 | ψ ]
--   ]
