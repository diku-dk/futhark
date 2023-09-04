def main [n][m] (k: i64) (xss: [n][m]i64) : [n]i64 =
  map (\xs ->
  loop s=0 for i < m / k
  do
    s + xs[i * k] -- inv?
  ) xss

-- === Expected output of analysis:
-- TBD