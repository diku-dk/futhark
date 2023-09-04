def main [n][m] (xss: [n][m]i64) : [n]i64 =
  map (reduce (+) 0) xss

-- === Expected output of analysis:
-- TBD