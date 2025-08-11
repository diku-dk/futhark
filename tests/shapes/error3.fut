-- The sizes of a lambda parameter can percolate out to a let-binding.
-- ==
-- error: "n" and "m" do not match

def f [n] (xs: [n]i32) = \(ys: [n]i32) -> (xs, ys)

def main [n] [m] (xs: [n]i32) (ys: [m]i32) = f xs ys
