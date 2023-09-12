def main [l][n][m] (xsss: [l][n][m]i64) : [l][n]i64 =
  map (\xss ->
    map (reduce (+) 0) xss
  ) xsss

-- === Expected output of analysis:
-- entry_main
-- TBD