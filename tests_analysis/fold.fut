def main [n][m] (xss: [n][m]i64) : [n]i64 =
  map (foldl (+) 0) xss

-- === Expected output of analysis:
-- entry_main
--   segmap_usable_groups_5203 []
--   eta_p_5207 [[σ gtid_5205 | ν | par], [σ xss_5144 | ν | seq]]