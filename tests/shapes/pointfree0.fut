-- Defunctionaliser didn't handle this point-free definition properly.

let f32id [n] (xs: [n]f32) : [n]f32 = xs

let median = f32id >-> id

entry median_entry xs = median xs
