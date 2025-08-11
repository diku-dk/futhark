-- The initial values for merge parameters must have the right size.
-- ==
-- error: \[n\]i32

def main [m] (xs: [m]i32) (n: i64) =
  loop (ys: [n]i32) = xs
  for _i < 3i32 do
    replicate n (ys[0] + 1)
