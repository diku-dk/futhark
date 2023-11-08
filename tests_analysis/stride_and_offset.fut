def main [n][m] (xss: [n][m]i64) : [n]i64 =
  map (\xs ->
    #[unsafe]
    loop s=0 for i < m do
      s + xs[i*2+3]
  ) xss
