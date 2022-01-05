-- Optimise away a particularly simple case of bounds checking.
-- ==
-- structure { Assert 0 }

def main [n] (xs: [n]i32) =
  loop acc = 0 for i < n do acc + xs[i]
