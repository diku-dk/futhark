-- Defunctionaliser didn't handle this point-free definition properly.

def f32id [n] (xs: [n]f32) : [n]f32 = xs

def median = f32id >-> id

entry median_entry xs = median xs
