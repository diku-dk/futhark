entry main [n] (xs: [n]i64) : [n]i64 =
  map ((+) 2) xs

-- === Expected output of analysis:
