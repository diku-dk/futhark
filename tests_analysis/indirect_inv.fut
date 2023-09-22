def main [n][m] (is: [m]i64) (xss: [n][m]i64) : [n]i64 =
  let k = 5
  in map (\i -> #[unsafe] xss[is[k],i] ) (iota n)


-- === Expected output of analysis:
