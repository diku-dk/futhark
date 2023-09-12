def main [n] (xs: [n]i32) : [n]i32 =
  let is = iota n
  in map (\i ->  #[unsafe] xs[is[i]] ) (iota n)

-- === Expected output of analysis:
-- entry_main

-- WARNING: Too heavily optimized!!!
