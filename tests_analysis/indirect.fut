def main [n][m] (is: [m]i64) (xss: [n][m]i64) : [n]i64 =
  map (\xs ->
  loop s=0 for i < m
  do
    #[unsafe]
    s + xs[ is[ i ] ]
  ) xss


-- === Expected output of analysis:
-- TBD