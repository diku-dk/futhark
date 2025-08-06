-- Defunctionaliser didn't handle this point-free definition properly.

entry median_entry xs =
  let f32id [n] (xs: [n]f32): [n]f32 = xs
  let median = f32id >-> id
  in median xs
