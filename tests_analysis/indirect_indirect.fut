def main [n] (xs: [n]i64) : [n]i64 =
  let is = iota n
  let is_2 = reverse is
  in map (\i -> #[unsafe] xs[is[is_2[i]]]) (iota n)

-- === Expected output of analysis:
-- TBD