entry main [n] (xs: [n]i64) : i64 =
  loop accumulator = 3 for i < n do
    #[unsafe]
    accumulator + xs[i]

-- === Expected output of analysis:
