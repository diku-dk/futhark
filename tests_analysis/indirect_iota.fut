def main [n] (xs: [n]i64) : [n]i64 =
  let is = iota n
  in map (\i -> #[unsafe] xs[is[i]]) (iota n)

-- === Expected output of analysis:
-- TBD