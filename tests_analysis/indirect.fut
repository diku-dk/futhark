def main [n][m] (is: [m]i64) (xss: [n][m]i64) : [n]i64 =
  map (\xs ->
    #[unsafe]
    loop s=0 for i < m
    do
      s + xs[ is[ i ] ]
  ) xss


-- === Expected output of analysis:
-- TBD